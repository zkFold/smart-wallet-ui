import { AppConfig, AppView, WalletState } from './Types'
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
    this.config = this.loadConfig()
    this.storage = new StorageManager()
    this.backendService = new BackendService(this.config)
    this.walletManager = new WalletManager(this.config, this.storage)
    this.router = new Router(this.backendService)

    // Set up event listeners
    this.setupEventListeners()
  }

  private loadConfig(): AppConfig {
    return {
      clientId: import.meta.env.VITE_CLIENT_ID || '',
      clientSecret: import.meta.env.VITE_CLIENT_SECRET || '',
      websiteUrl: import.meta.env.VITE_WEBSITE_URL || window.location.origin,
      backendUrl: import.meta.env.VITE_BACKEND_URL || '',
      backendApiKey: import.meta.env.VITE_BACKEND_API_KEY
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

    this.walletManager.on('transactionFailed', (event: any) => {
      this.router.navigate('failed', { reason: event.data.message })
    })

    // Listen for router navigation
    this.router.on('navigate', (event: any) => {
      this.currentView = event.data.view
      this.render()
    })

    // Handle browser back/forward buttons
    window.addEventListener('popstate', () => {
      this.handleUrlChange()
    })
  }

  private handleUrlChange(): void {
    const path = window.location.pathname
    const params = new URLSearchParams(window.location.search)

    // Handle OAuth callback - authorization code flow
    if (params.has('code')) {
      this.walletManager.handleOAuthCallback(window.location.search)
      return
    }

    // Handle other routes
    if (path === '/wallet' && this.walletState.isInitialized) {
      this.router.navigate('wallet')
    } else {
      this.router.navigate('init')
    }
  }

  public async init(): Promise<void> {
    try {
      // Try to restore wallet state from active wallet in storage
      const activeWallet = this.storage.getActiveWallet()
      if (activeWallet && activeWallet.state.isInitialized) {
        this.walletState = activeWallet.state
        await this.walletManager.restoreWallet(activeWallet.state)
      }

      // Handle initial routing
      this.handleUrlChange()

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
    }
  }

  private setupInitHandlers(): void {
    const form = document.querySelector('form') as HTMLFormElement
    if (form) {
      form.addEventListener('submit', async (e) => {
        e.preventDefault()
        const formData = new FormData(form)
        const network = formData.get('network') as string

        await this.walletManager.initializeWallet({
          method: 'Google Oauth',
          network: network as any,
          data: undefined
        })
      })
    }

    // Handle show controls button
    const showControlsBtn = document.getElementById('show_controls')
    if (showControlsBtn) {
      showControlsBtn.addEventListener('click', () => {
        this.toggleAdvancedControls()
      })
    }


  }

  private setupWalletHandlers(): void {
    const form = document.querySelector('form') as HTMLFormElement
    if (form) {
      form.addEventListener('submit', async (e) => {
        e.preventDefault()
        const formData = new FormData(form)

        await this.walletManager.sendTransaction({
          recipient: formData.get('zkfold_address') as string,
          recipientType: formData.get('recipient') as any,
          amount: formData.get('zkfold_amount') as string,
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
  }

  // UI helper methods (keeping existing functionality)
  private toggleAdvancedControls(): void {
    const header = document.getElementById("header")
    const networkSel = document.getElementById("network_selector")
    const networkOpt = document.getElementById("network_option") as HTMLSelectElement
    const button = document.getElementById("show_controls")

    if (header && networkSel && button) {
      if (networkSel.hidden) {
        header.innerHTML = "Smart Wallet"
        networkSel.hidden = false
        button.innerHTML = "Hide advanced controls"
      } else {
        header.innerHTML = "Smart Wallet (Preprod)"
        networkSel.hidden = true
        if (networkOpt) networkOpt.value = "Preprod"
        button.innerHTML = "Show advanced controls"
      }
    }
  }



  private updateTypeUI(): void {
    const addressInput = document.getElementById("address_input") as HTMLInputElement
    const selector = document.getElementById("type_selector") as HTMLSelectElement

    if (addressInput && selector) {
      if (selector.value === "Gmail") {
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
        if (selector) selector.value = "Gmail"
        assetName.hidden = true
        this.updateTypeUI()
      }
    }
  }
}