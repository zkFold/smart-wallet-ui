import { AppConfig, AppView } from './Types'
import { renderInitView } from './UI/Init'
import { renderWalletView } from './UI/Wallet'
import { renderFailedView } from './UI/Failed'
import { renderSuccessView } from './UI/Success'
import { AddressType, Backend, GoogleApi, Prover, Wallet } from 'zkfold-smart-wallet-api'

export class App {
  private wallet!: Wallet

  constructor() {
    const config: AppConfig = {
        clientId: import.meta.env.VITE_CLIENT_ID,
        clientSecret: import.meta.env.VITE_CLIENT_SECRET,
        websiteUrl: import.meta.env.VITE_WEBSITE_URL,
        backendUrl: import.meta.env.VITE_BACKEND_URL,
        backendApiKey: import.meta.env.VITE_BACKEND_API_KEY,
        proverUrl: import.meta.env.VITE_PROVER_URL,
      }

    const googleApi = new GoogleApi(config.clientId, config.clientSecret, `${config.websiteUrl}/oauth2callback`)
    const backend = new Backend(config.backendUrl, config.backendApiKey)
    const prover = new Prover(config.proverUrl)

    this.wallet = new Wallet(googleApi, backend, prover)
  }

  private async setupNavigation(): Promise<void> {
    // Listen for wallet state changes
    this.wallet.addEventListener('walletInitialized', async () => {
      await this.render('wallet')
    })

    this.wallet.addEventListener('transactionComplete', async (event: Event) => {
      await this.render('success', (event as CustomEvent).detail)
    })

    this.wallet.addEventListener('proofComputationComplete', async (event: Event) => {
      await this.render('success', (event as CustomEvent).detail)
    })

    this.wallet.addEventListener('transactionFailed', async (event: Event) => {
      const detail = (event as CustomEvent).detail
      await this.render('failed', { reason: detail?.message ?? detail })
    })

    this.wallet.addEventListener('walletLoggedOut', async () => {
      await this.render('init')
    })
  }

  public async init(): Promise<void> {
    try {
      // Set up event listeners
      this.setupNavigation()

      // Check for OAuth callback first
      const params = new URLSearchParams(window.location.search)
      if (params.has('code')) {
        await this.wallet.oauthCallback(window.location.search)
        return
      } else {
        // Initial render
        await this.render('init')
      }
    } catch (error) {
      console.error('Failed to initialize app:', error)
      await this.render('init')
    }
  }

  private async render(view: AppView, data?: any): Promise<void> {
    const appElement = document.getElementById('app')
    if (!appElement) {
      console.error('App element not found')
      throw new Error('App element not found')
    }

    // Clear previous content
    appElement.innerHTML = ''

    // Render current view
    let viewElement: HTMLElement
    switch (view) {
      case 'init':
        viewElement = renderInitView()
        break
      case 'wallet':
        const userId = await this.wallet.getUserId()
        const address = await this.wallet.getAddress().then((x: any) => x.to_bech32())
        const balance = await this.wallet.getBalance()
        viewElement = renderWalletView(userId, address, balance)
        break
      case 'success':
        viewElement = renderSuccessView(this.wallet, data)
        break
      case 'failed':
        viewElement = renderFailedView(data)
        break
      default:
        viewElement = renderInitView()
    }

    appElement.appendChild(viewElement)

    // Set up event handlers for the current view
    this.setupViewEventHandlers(view)
  }

  private setupViewEventHandlers(view: AppView): void {
    // Handle form submissions and button clicks based on current view
    switch (view) {
      case 'init':
        this.setupInitHandlers()
        break
      case 'wallet':
        this.setupWalletHandlers()
        break
      case 'success':
        this.setupPostTxHandlers()
        break
      case 'failed':
        this.setupPostTxHandlers()
        break
    }
  }

  private setupInitHandlers(): void {
    const form = document.querySelector('form') as HTMLFormElement
    if (form) {
      form.addEventListener('submit', async (e) => {
        e.preventDefault()
        this.wallet.login()
      })
    }
  }

  private setupWalletHandlers(): void {
    const form = document.querySelector('form') as HTMLFormElement
    if (form) {
      form.addEventListener('submit', async (e) => {
        e.preventDefault()
        const formData = new FormData(form)

        // Get the amount in ADA and convert to lovelace
        const adaAmount = parseFloat(formData.get('zkfold_amount') as string)
        if (Number.isNaN(adaAmount)) {
          console.error('Invalid amount provided')
          return
        }
        const lovelaceAmount = Math.round(adaAmount * 1_000_000).toString()

        const recipient = (formData.get('zkfold_address') as string).trim()
        const recipientType = this.detectRecipientType(recipient)
        const asset = (formData.get('zkfold_asset') as string)?.trim() || 'lovelace'

        await this.wallet.sendTransaction({
          recipient,
          recipientType,
          amount: lovelaceAmount,
          asset
        })
      })
    }

    const logoutBtn = document.getElementById('logout_button')
    if (logoutBtn) {
      logoutBtn.addEventListener('click', () => {
        this.wallet.logout()
      })
    }
  }

  private setupPostTxHandlers(): void {
    const retryBtn = document.getElementById('new_tx')
    const logoutBtn = document.getElementById('logout_button') as HTMLButtonElement | null

    if (retryBtn) {
      retryBtn.removeAttribute('disabled')
      retryBtn.onclick = async () => {
        retryBtn.setAttribute('disabled', 'true')
        await this.render('wallet')
      }
    }

    if (logoutBtn) {
      logoutBtn.removeAttribute('disabled')
      logoutBtn.onclick = async () => {
        logoutBtn.setAttribute('disabled', 'true')
        this.wallet.logout()
      }
    }
  }

  // UI helper methods (keeping existing functionality)
  private detectRecipientType(address: string): AddressType {
    const value = address.trim()

    if (!value) {
      return AddressType.Email
    }

    if (value.includes('@')) {
      return AddressType.Email
    }

    return AddressType.Bech32
  }
}
