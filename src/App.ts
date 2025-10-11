import { AppConfig, AppView } from './Types'
import { WalletManager } from './Wallet/WalletManager'
import { Router } from './UI/Router'
import { StorageManager } from './Utils/Storage'
import { BackendService } from './Services/BackendService'

export class App {
  private config: AppConfig
  private walletManager!: WalletManager
  private router: Router
  private storage: StorageManager
  private backendService: BackendService
  private currentView: AppView = 'init'

  constructor() {
    // Load configuration from environment or defaults
    this.config = this.loadConfig()
    this.storage = new StorageManager()
    this.backendService = new BackendService(this.config)
    this.router = new Router(this.backendService)
  }

  private loadConfig(): AppConfig {
    return {
      clientId: import.meta.env.VITE_CLIENT_ID || '',
      clientSecret: import.meta.env.VITE_CLIENT_SECRET || '',
      websiteUrl: import.meta.env.VITE_WEBSITE_URL || window.location.origin,
      backendUrl: import.meta.env.VITE_BACKEND_URL || '',
      backendApiKey: import.meta.env.VITE_BACKEND_API_KEY,
      proverUrl: import.meta.env.VITE_PROVER_URL || '',
    }
  }

  private async setupEventListeners(): Promise<void> {
    // Listen for wallet state changes
    this.walletManager.on('walletInitialized', async () => {
      await this.render('wallet')
    })

    this.walletManager.on('transactionComplete', async (event: any) => {
      await this.render('success', event.data)
    })

    this.walletManager.on('proofComputationComplete', async (event: any) => {
      // Update the success view to show transaction pending instead of proof computing
      await this.router.updateProofComputationComplete(event.data.txId, event.data.recipient)
    })

    this.walletManager.on('transactionFailed', async (event: any) => {
      await this.render('failed', { reason: event.data.message })
    })

    this.walletManager.on('walletLoggedOut', async () => {
      await this.render('init')
    })
  }

  public async init(): Promise<void> {
    try {
      const credentials = await this.backendService.credentials()
      if (!credentials) {
        throw new Error("Google Client credentials are bull")
      }

      if (this.config.clientId === '' || this.config.clientSecret === '') {
        this.config.clientId = credentials.client_id
        this.config.clientSecret = credentials.client_secret
      }

      this.walletManager = new WalletManager(this.config, this.storage)

      // Set up event listeners
      this.setupEventListeners()

      // Check for OAuth callback first
      const params = new URLSearchParams(window.location.search)
      if (params.has('code')) {
        await this.walletManager.oauthCallback(window.location.search)
        return
      }

      if (this.walletManager.isLoggedIn()) {
        await this.render('wallet')
      } else {
        await this.render('init')
      }

      // Initial render
      await this.render('init')
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
        viewElement = this.router.renderInitView()
        break
      case 'wallet':
        const userId = await this.walletManager.getUserId()
        const address = await this.walletManager.getWalletAddress()
        const balance = await this.walletManager.getWalletBalance()
        viewElement = this.router.renderWalletView(userId, address, balance)
        break
      case 'success':
        viewElement = this.router.renderSuccessView(data)
        break
      case 'failed':
        viewElement = this.router.renderFailedView(data)
        break
      default:
        viewElement = this.router.renderInitView()
    }

    appElement.appendChild(viewElement)

    // Set up event handlers for the current view
    this.setupViewEventHandlers()
  }

  private setupViewEventHandlers(): void {
    // Handle form submissions and button clicks based on current view
    switch (this.currentView) {
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
        this.walletManager.login()
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
        const lovelaceAmount = Math.round(adaAmount * 1_000_000).toString()

        await this.walletManager.sendTransaction({
          recipient: formData.get('zkfold_address') as string,
          recipientType: parseInt(formData.get('recipient') as string),
          amount: lovelaceAmount,
          asset: formData.get('zkfold_asset') as string || 'lovelace'
        })
      })
    }

    // Handle UI toggle buttons
    const showAddressBtn = document.getElementById('show_address')
    if (showAddressBtn) {
      showAddressBtn.addEventListener('click', () => {
        this.toggleAddressDisplay()
      })
    }

    const showSelectorBtn = document.getElementById('show_selector')
    if (showSelectorBtn) {
      showSelectorBtn.addEventListener('click', () => {
        this.toggleSelector()
      })
    }

    const typeSelect = document.getElementById('type_selector')
    if (typeSelect) {
      typeSelect.addEventListener('change', () => {
        this.updateTypeUI()
      })
    }

    const logoutBtn = document.getElementById('logout_button')
    if (logoutBtn) {
      logoutBtn.addEventListener('click', () => {
        this.walletManager.logout()
      })
    }
  }

  private setupPostTxHandlers(): void {
    const retryBtn = document.getElementById('new_tx')
    const newWalletBtn = document.getElementById('new_wallet')

    if (retryBtn) {
      retryBtn.removeAttribute('disabled')
      retryBtn.onclick = async () => {
        retryBtn.setAttribute('disabled', 'true')
        await this.render('wallet')
      }
    }

    if (newWalletBtn) {
      newWalletBtn.removeAttribute('disabled')
      newWalletBtn.onclick = async () => {
        newWalletBtn.setAttribute('disabled', 'true')
        this.walletManager.logout()
        await this.render('init')
      }
    }
  }

  // UI helper methods (keeping existing functionality)
  private updateTypeUI(): void {
    const addressInput = document.getElementById("address_input") as HTMLInputElement
    const selector = document.getElementById("type_selector") as HTMLSelectElement

    if (addressInput && selector) {
      if (selector.value === "1") { // AddressType.Email
        addressInput.placeholder = "example@gmail.com"
      } else {
        addressInput.placeholder = "addr_test1xyz...(Bech32)"
      }
    }
  }

  private toggleAddressDisplay(): void {
    const label = document.getElementById("faucet_label")
    const button = document.getElementById("show_address")

    if (label && button) {
      if (label.hidden) {
        label.hidden = false
        button.innerHTML = "Hide address"
      } else {
        label.hidden = true
        button.innerHTML = "Show address"
      }
    }
  }

  private toggleSelector(): void {
    const label = document.getElementById("address_type")
    const button = document.getElementById("show_selector")
    const selector = document.getElementById("type_selector") as HTMLSelectElement
    const assetName = document.getElementById("asset_name")

    if (label && button && assetName) {
      if (label.hidden) {
        label.hidden = false
        button.innerHTML = "Hide address selector"
        assetName.hidden = false
      } else {
        label.hidden = true
        button.innerHTML = "Show all controls"
        if (selector) selector.value = "1" // AddressType.Email
        assetName.hidden = true
        this.updateTypeUI()
      }
    }
  }
}
