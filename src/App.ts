import { AppView } from './Types'
import * as CSL from '@emurgo/cardano-serialization-lib-asmjs'
import { renderInitView } from './UI/Init'
import { renderWalletView } from './UI/Wallet'
import { renderErrorView } from './UI/Error'
import { Backend, GoogleApi, PopupWallet, Prover, Wallet, AddressType, AbstractWallet } from 'zkfold-smart-wallet-api'
import { AssetMetadataMap, buildAssetMetadata, formatAssetOptions, formatBalance, formatWithDecimals } from './Utils/Assets'
import { getAddressLabel } from './Utils/Address'
import { AppConfig } from './Types'

export class App {
  public wallet!: AbstractWallet
  private balanceRefreshInterval: number | null = null
  private assetMetadata: AssetMetadataMap = {
    lovelace: {
      label: 'ADA',
      decimals: 6
    }
  }

  constructor(wallet: AbstractWallet) {
    this.wallet = wallet
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
      if (params.has('code') && !App.isBrowserExtension()) {
        await this.wallet.oauthCallback(window.location.search)
        return
      } else {
        // Initial render
        await this.render('init')
        if (App.isBrowserExtension()) {
          chrome.storage.local.get(['jwt', 'tokenSKey', 'userId'], async (result) => {
            if (result.jwt && result.tokenSKey && result.userId) {
              this.wallet.jwt = result.jwt as string;
              this.wallet.tokenSKey = CSL.Bip32PrivateKey.from_hex(result.tokenSKey as string);
              this.wallet.userId = result.userId as string;
              this.wallet.dispatchEvent(new CustomEvent('initialized'))
            }
          });
        }
      }
    } catch (error) {
      console.error('Failed to initialize app:', error)
      await this.render('error')
      this.showNotification('Error!', 'Something went wrong.', 'error')
    }
  }

  public static async createFromEnv(): Promise<App> {
    const envProverUrls = import.meta.env.VITE_PROVER_URL
    const proverUrls = envProverUrls
      .split(',')
      .map((url) => url.trim())
      .filter((url) => url.length > 0)

    // Randomize the prover on each load to balance requests across available endpoints

    async function getRandomHealthyUrl(urlArray: string[]): Promise<string> {
      const healthyUrls: string[] = [];

      const checkURLs = urlArray.map(async (url: string) => {
        const prover = new Prover(url)
        try {
          await prover.serverKeys()
          healthyUrls.push(url)
        }
        catch {
          console.log("Prover not available")
        }
      })
      await Promise.all(checkURLs)

      if (healthyUrls.length === 0) {
        throw new Error("No healthy URLs found");
      }

      return healthyUrls[Math.floor(Math.random() * healthyUrls.length)];
    }

    const selectedProverUrl = await getRandomHealthyUrl(proverUrls);

    const config: AppConfig = {
      websiteUrl: import.meta.env.VITE_WEBSITE_URL,
      backendUrl: import.meta.env.VITE_BACKEND_URL,
      backendApiKey: import.meta.env.VITE_BACKEND_API_KEY,
      proverUrl: selectedProverUrl,
    }

    const backend = new Backend(config.backendUrl, config.backendApiKey)
    const prover = new Prover(config.proverUrl)

    const creds = await backend.credentials()

    let wallet;
    if (App.isBrowserExtension()) {
      const redirectUrl = chrome.identity.getRedirectURL() + "oauth2callback/";
      wallet = new PopupWallet(backend, prover, new GoogleApi(creds.client_id, creds.client_secret, redirectUrl))
    } else {
      wallet = new Wallet(backend, prover, new GoogleApi(creds.client_id, creds.client_secret, `${config.websiteUrl}/oauth2callback/`))
    }
    const app = new App(wallet)
    return app
  }

  private async render(view: AppView): Promise<void> {
    // Clear previous content
    const app = document.getElementById('app') as HTMLElement
    app.innerHTML = ''

    // Clear any active balance polling when switching views
    this.clearBalanceRefreshInterval()

    // Render current view
    let viewElement: HTMLElement
    switch (view) {
      case 'wallet':
        const userId = this.wallet.getUserId()
        const address = await this.wallet.getAddress().then((x: any) => x.to_bech32())
        const balance = await this.wallet.getBalance()
        this.assetMetadata = buildAssetMetadata(balance)
        const txHistory = await this.wallet.getTxHistory()
        viewElement = renderWalletView(userId, address, balance, txHistory, this.wallet.isActivated(), this.assetMetadata)
        app.appendChild(viewElement)
        this.setupWalletHandlers(userId, address)
        break
      case 'error':
        viewElement = renderErrorView()
        app.appendChild(viewElement)
        break
      default:
        viewElement = renderInitView()
        app.appendChild(viewElement)
        this.setupInitHandlers()
    }
  }

  public static isBrowserExtension(): boolean {
    const isChromeExtension = typeof chrome !== 'undefined' &&
      typeof chrome.runtime !== 'undefined';

    return isChromeExtension;
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
    const setSendLoading = (loading: boolean) => {
      const btn = document.querySelector('.wallet_sec .submit_btn') as HTMLButtonElement | null
      if (!btn) return
      btn.disabled = loading
      btn.classList.toggle('loading', loading)
      if (loading) {
        // preserve original label once
        if (!btn.dataset.label) {
          btn.dataset.label = (btn.textContent || 'Send').trim()
        }
        if (this.wallet.hasProof()) {
          btn.textContent = 'Sending...'
        }
        else {
          btn.textContent = 'Computing ZK proof...'
        }
      } else {
        const original = btn.dataset.label || 'Send'
        btn.textContent = original
      }
    }
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

        // Start loading immediately on submit (after basic validation)
        setSendLoading(true)
        await this.wallet.sendTransaction({
          recipient,
          recipientType,
          amount: lovelaceAmount,
          asset
        })
        form.reset()
      })
    }

    // Transaction notifications
    this.wallet.addEventListener('transaction_pending', async (event: Event) => {
      setSendLoading(false)
      const request = (event as CustomEvent).detail
      const assetDetails = this.assetMetadata[request.asset]
      if (!assetDetails) {
        return
      }
      const rawAmount = Number(request.amount)
      if (Number.isNaN(rawAmount)) {
        console.warn('Unable to parse transaction amount', request.amount)
        return
      }
      const amt = formatWithDecimals(rawAmount, assetDetails.decimals)
      const asset = assetDetails.label
      const recipient = getAddressLabel(request.recipient)
      this.showNotification("Success!", `Sent ${amt} ${asset} to ${recipient}.`, 'success')
    })
    this.wallet.addEventListener('transaction_failed', async (event: Event) => {
      setSendLoading(false)
      const errorMsg = (event as CustomEvent).detail
      this.showNotification("Failed!", errorMsg, 'error')
    })
    this.wallet.addEventListener('transaction_confirmed', async (_event: Event) => {
      this.refreshBalance()
    })

    // Kick off periodic balance refreshes
    this.balanceRefreshInterval = window.setInterval(() => {
      this.refreshBalance()
    }, 60_000)

    const logoutBtn = document.getElementById('logout_button')
    if (logoutBtn) {
      logoutBtn.addEventListener('click', () => {
        this.wallet.logout()
      })
    }

    // Make the entire header clickable to toggle the panel
    for (const header of Array.from(document.querySelectorAll('.wallet_sec .wallet_box .wallet_box_header'))) {
      header.addEventListener('click', () => {
        const walletBox = header.closest('.wallet_box')
        walletBox?.classList.toggle('active')
      })
    }

    for (const listItem of Array.from(document.querySelectorAll('.wallet_sec .wallet_detail_list .wallet_detail_list__item-btn'))) {
      listItem.addEventListener('click', () => {
        const item = listItem.closest('.wallet_detail_list__item')
        const content = item?.querySelector('.wallet_detail_list__details') as HTMLElement | null
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
          this.showNotification("Copied!", 'Email copied to clipboard.', 'info')
        })
      }

      const copyTopupAddressBtn = document.getElementById('copy_topup_adress')
      if (copyTopupAddressBtn) {
        copyTopupAddressBtn.addEventListener('click', async () => {
          await navigator.clipboard.writeText(address)
          this.showNotification("Copied!", 'Address copied to clipboard.', 'info')
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

  private async refreshBalance(): Promise<void> {
    try {
      const newBalance = await this.wallet.getBalance()
      this.assetMetadata = buildAssetMetadata(newBalance)
      const hasAssets = Object.keys(newBalance).length > 0

      const assetsList = document.getElementById('wallet_assets_list') as HTMLUListElement | null
      const assetsWrap = assetsList?.parentElement as HTMLElement | null
      if (assetsList && assetsWrap) {
        if (hasAssets) {
          assetsList.innerHTML = formatBalance(newBalance)
          assetsWrap.classList.remove('empty')
        } else {
          assetsList.innerHTML = ''
          assetsWrap.classList.add('empty')
        }
      }

      const select = document.getElementById('sendto_asset_select') as HTMLSelectElement | null
      if (select) {
        const selectedBefore = select.value
        select.innerHTML = formatAssetOptions(newBalance)
        const stillExists = Array.from(select.options).some(o => o.value === selectedBefore)
        if (stillExists) select.value = selectedBefore
      }
    } catch (err) {
      console.error('Failed to refresh balance:', err)
    }
  }

  private clearBalanceRefreshInterval(): void {
    if (this.balanceRefreshInterval !== null) {
      clearInterval(this.balanceRefreshInterval)
      this.balanceRefreshInterval = null
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

  private async showNotification(header: string, body: string, type: 'info' | 'success' | 'error' = 'info'): Promise<void> {
    const notification = document.getElementById('notification')
    const notificationHeader = document.getElementById('notification_header')
    const notificationBody = document.getElementById('notification_body')
    const notificationTimeoutId = document.getElementById('notification_timeout_id') as HTMLInputElement

    if (notification && notificationHeader && notificationBody && notificationTimeoutId) {
      // Clear any existing timeout
      const existingTimeoutId = notificationTimeoutId.value
      clearTimeout(existingTimeoutId)

      // Update type class
      notification.classList.remove('error', 'success')
      if (type === 'error') {
        notification.classList.add('error')
      } else if (type === 'success') {
        notification.classList.add('success')
      }

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
