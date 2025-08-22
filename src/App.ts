import { AppConfig, AppView, Network, WalletState } from './Types'
import { ClientCredentials } from 'zkfold-smart-wallet-api'
import { WalletManager } from './Wallet/WalletManager'
import { Router } from './UI/Router'
import { StorageManager } from './Utils/Storage'
import { BackendService } from './Services/BackendService'

export class App {
  private config: AppConfig
  private walletManager: WalletManager
  private router: Router
  private storage: StorageManager
  private backendService: BackendService
  private currentView: AppView = 'init'
  private walletState: WalletState = { isInitialized: false }

  constructor() {
    // Load configuration from environment or defaults
    this.config = this.loadConfig();
    this.storage = new StorageManager();
    this.backendService = new BackendService(this.config);
    this.router = new Router(this.backendService);
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

  private setupEventListeners(): void {
    // Listen for wallet state changes
    this.walletManager.on('walletInitialized', (event: any) => {
      this.walletState = event.data
      this.router.navigate('wallet')
    })

    this.walletManager.on('transactionComplete', (event: any) => {
      this.router.navigate('success', event.data)
    })

    this.walletManager.on('proofComputationComplete', (event: any) => {
      // Update the success view to show transaction pending instead of proof computing
      this.router.updateProofComputationComplete(event.data.txId, event.data.recipient)
    })

    this.walletManager.on('transactionFailed', (event: any) => {
      this.router.navigate('failed', { reason: event.data.message })
    })

    this.walletManager.on('walletLoggedOut', () => {
      this.router.navigate('init')
    })

    // Listen for router navigation
    this.router.on('navigate', (event: any) => {
      this.currentView = event.data.view
      this.render()
    })

    // Listen for refresh and navigate events
    this.router.on('refreshAndNavigate', async (event: any) => {
      const targetView = event.data
      console.log(`TARGET VIEW: ${targetView}`)
      if (targetView === 'wallet') {
        try {
          // Refresh wallet state before navigating
          this.walletState = await this.walletManager.refreshWalletState()
          this.currentView = 'wallet'
          this.render()
        } catch (error) {
          console.error('Failed to refresh wallet state:', error)
          // Fallback to regular navigation
          this.router.navigate('wallet')
        }
      } else {
        this.router.navigate(targetView)
      }
    })
  }

  public async init(clientName: string): Promise<void> {
    try {
      const credentials = await this.backendService.credentials(clientName);

      if (this.config.clientId === '' || this.config.clientSecret === '') {
        this.config.clientId = credentials.client_id;
        this.config.clientSecret = credentials.client_secret;
      }

      this.walletManager = new WalletManager(this.config, this.storage);

      // Set up event listeners
      this.setupEventListeners();

      // Check for OAuth callback first
      const params = new URLSearchParams(window.location.search)
      if (params.has('code')) {
        await this.walletManager.handleOAuthCallback(window.location.search)
        return
      }

      // Try to restore wallet state from active wallet in storage
      const activeWallet = this.storage.getActiveWallet()
      if (activeWallet && activeWallet.state.isInitialized) {
        this.walletState = activeWallet.state
        await this.walletManager.restoreWallet(activeWallet.state)
        this.router.navigate('wallet')
      } else {
        this.router.navigate('init')
      }

      // Initial render
      this.render()
    } catch (error) {
      console.error('Failed to initialize app:', error)
      this.router.navigate('init')
    }
  }

  private render(): void {
    const appElement = document.getElementById('app')
    if (!appElement) {
      console.error('App element not found')
      throw new Error('App element not found')
    }

    // Clear previous content
    appElement.innerHTML = ''

    // Render current view
    let viewElement: HTMLElement
    switch (this.currentView) {
      case 'init':
        viewElement = this.router.renderInitView()
        break
      case 'wallet':
        viewElement = this.router.renderWalletView(this.walletState)
        break
      case 'success':
        viewElement = this.router.renderSuccessView(this.router.getViewData())
        break
      case 'failed':
        viewElement = this.router.renderFailedView(this.router.getViewData())
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
        const formData = new FormData(form)
        const network = formData.get('network') as Network

        await this.walletManager.initializeWallet(network)
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
      retryBtn.addEventListener('click', async () => {
        try {
          // Refresh wallet state before navigating
          this.walletState = await this.walletManager.refreshWalletState()
          this.currentView = 'wallet'
          this.render()
        } catch (error) {
          console.error('Failed to refresh wallet state:', error)
          // Fallback to regular navigation
          this.router.navigate('wallet')
        }
      })
    }

    if (newWalletBtn) {
      newWalletBtn.removeAttribute('disabled')
      newWalletBtn.addEventListener('click', () => {
        this.walletManager.logout()
        this.router.navigate('init')
      })
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
