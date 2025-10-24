import { AppView } from './Types'
import { renderInitView } from './UI/Init'
import { renderWalletView } from './UI/Wallet'
import { AddressType, Backend, GoogleApi, Prover, Wallet } from 'zkfold-smart-wallet-api'

export class App {
  private wallet!: Wallet

  constructor(backend: Backend, prover: Prover, googleApi: GoogleApi) {
    this.wallet = new Wallet(backend, prover, googleApi)
  }

  public async init(): Promise<void> {
    try {
      // Set up event listeners
      this.wallet.addEventListener('initialized', async () => {
        await this.render('wallet')
      })

      this.wallet.addEventListener('logged_out', async () => {
        await this.render('init')
      })

      // Check for OAuth callback
      const params = new URLSearchParams(window.location.search)
      if (params.has('code')) {
        await this.wallet.oauthCallback(window.location.search)
        return
      } else {
        // Initial render
        await this.render('init')
      }
    } catch (error) {
      // TODO: We should have an error view to render here
      console.error('Failed to initialize app:', error)
      await this.render('init')
    }
  }

  private async render(view: AppView): Promise<void> {
    // Clear previous content
    const app = document.getElementById('app') as HTMLElement
    app.innerHTML = ''

    // Render current view
    let viewElement: HTMLElement
    switch (view) {
      case 'wallet':
        const userId = this.wallet.getUserId()
        const address = await this.wallet.getAddress().then((x: any) => x.to_bech32())
        const balance = await this.wallet.getBalance()
        viewElement = renderWalletView(userId, address, balance)
        app.appendChild(viewElement)
        this.setupWalletHandlers(userId, address)
        break
      default:
        viewElement = renderInitView()
        app.appendChild(viewElement)
        this.setupInitHandlers()
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

  private setupWalletHandlers(userId: string, address: string): void {
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

    // Transaction notifications
    this.wallet.addEventListener('transaction_pending', async (event: Event) => {
      const txId = (event as CustomEvent).detail
      this.showNotification("Success!", `Transaction ${txId} has been submitted.`)
    })
    this.wallet.addEventListener('transaction_failed', async (event: Event) => {
      const error = (event as CustomEvent).detail
      this.showNotification("Failed!", `Transaction failed: ${error}`)
    })

    const logoutBtn = document.getElementById('logout_button')
    if (logoutBtn) {
      logoutBtn.addEventListener('click', () => {
        this.wallet.logout()
      })
    }

    for (const showBtn of Array.from(document.querySelectorAll(".wallet_sec .toggle_btn"))) {
      showBtn.addEventListener('click', () => {
        const walletBox = showBtn.closest('.wallet_box')
        walletBox?.classList.toggle('active')
      })
    }

    for (const listItem of Array.from(document.querySelectorAll('.wallet_sec .price_list .price_list_item_btn'))) {
      listItem.addEventListener('click', () => {
        const item = listItem.closest('.price_list_item')
        const content = item?.querySelector('.price_list_item_value') as HTMLElement | null
        if (content) {
          content.style.display = content.style.display === 'none' ? 'block' : 'none'
        }
      })
    }

    // Add copy functionality
    setTimeout(() => {
      const copyEmailBtn = document.getElementById('copy_email')
      if (copyEmailBtn) {
        copyEmailBtn.addEventListener('click', async () => {
          await navigator.clipboard.writeText(userId)
          this.showNotification("Copied!", 'Email copied to clipboard')
        })
      }

      const copyTopupAddressBtn = document.getElementById('copy_topup_adress')
      if (copyTopupAddressBtn) {
        copyTopupAddressBtn.addEventListener('click', async () => {
          await navigator.clipboard.writeText(address)
          this.showNotification("Copied!", 'Address copied to clipboard')
        })
      }

      const copyCloseIcon = document.getElementById('notification_close_icon')
      if (copyCloseIcon) {
        copyCloseIcon.addEventListener('click', () => {
          const notification = document.getElementById('notification')
          if (notification) {
            notification.classList.remove('active')
          }
        })
      }
    }, 0)
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

  private async showNotification(header: string, body: string): Promise<void> {
    const notification = document.getElementById('notification')
    const notificationHeader = document.getElementById('notification_header')
    const notificationBody = document.getElementById('notification_body')
    const notificationTimeoutId = document.getElementById('notification_timeout_id') as HTMLInputElement

    if (notification && notificationHeader && notificationBody && notificationTimeoutId) {
      // Clear any existing timeout
      const existingTimeoutId = notificationTimeoutId.value
      clearTimeout(existingTimeoutId)

      // Update message header
      notificationHeader.textContent = header

      // Update message body
      notificationBody.textContent = body

      // Show notification
      if (notification.classList.contains('active')) {
        notification.classList.remove('active')
        setTimeout(() => {
          notification.classList.add('active')
        }, 100)
      } else {
        notification.classList.add('active')
      }

      // Hide after 7 seconds
      const newTimeoutId = setTimeout(() => {
        notification.classList.remove('active')
      }, 7000)

      // Store new timeout ID
      notificationTimeoutId.value = newTimeoutId.toString()
    }
  }
}
