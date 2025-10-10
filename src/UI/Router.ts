import { AppView, WalletBalance } from '../Types'
import { EventEmitter } from '../Utils/EventEmitter'
import { formatBalance } from '../Utils/Helpers'
import { BackendService } from '../Services/BackendService'
import { AddressType } from 'zkfold-smart-wallet-api'

export class Router extends EventEmitter {
  private currentViewData: any = null
  private backendService?: BackendService

  constructor(backendService?: BackendService) {
    super()
    this.backendService = backendService
  }

  public setBackendService(backendService: BackendService): void {
    this.backendService = backendService
  }

  public navigate(view: AppView, data?: any): void {
    this.currentViewData = data
    this.emit('navigate', { view, data })
  }

  public getViewData(): any {
    return this.currentViewData
  }

  public renderInitView(): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'

    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <h3 id="header" style="text-align: center; font-weight: 400;">Create your wallet with email</h3>
      <form action="#" method="POST">
        <fieldset>
          <label id="network_selector" hidden>
            Network type
            <select name="network" id="network_option" aria-label="Select network" required>
              <option>Preview</option>
              <option selected="selected">Preprod</option>
            </select>
          </label>


        </fieldset>
        <button
          id="submit"
          type="submit"
          style="display: flex; align-items: center; justify-content: center; gap: 0.5rem;"
        >
          <svg width="18" height="18" viewBox="0 0 18 18" xmlns="http://www.w3.org/2000/svg">
            <g fill="none" fill-rule="evenodd">
              <path d="M17.64 9.205c0-.639-.057-1.252-.164-1.841H9v3.481h4.844a4.14 4.14 0 0 1-1.796 2.716v2.259h2.908c1.702-1.567 2.684-3.875 2.684-6.615z" fill="#4285F4"/>
              <path d="M9 18c2.43 0 4.467-.806 5.956-2.18l-2.908-2.259c-.806.54-1.837.86-3.048.86-2.344 0-4.328-1.584-5.036-3.711H.957v2.332A8.997 8.997 0 0 0 9 18z" fill="#34A853"/>
              <path d="M3.964 10.71A5.41 5.41 0 0 1 3.682 9c0-.593.102-1.17.282-1.71V4.958H.957A8.996 8.996 0 0 0 0 9c0 1.452.348 2.827.957 4.042l3.007-2.332z" fill="#FBBC05"/>
              <path d="M9 3.58c1.321 0 2.508.454 3.44 1.345l2.582-2.58C13.463.891 11.426 0 9 0A8.997 8.997 0 0 0 .957 4.958L3.964 7.29C4.672 5.163 6.656 3.58 9 3.58z" fill="#EA4335"/>
            </g>
          </svg>
          Continue with Google
        </button>
      </form>
    `

    return container
  }

  public renderWalletView(userId: string, address: string, balance: WalletBalance): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'

    const userIdHtml = userId || 'Unknown'
    const addressHtml = address || 'Loading...'
    const balanceHtml = balance ? formatBalance(balance) : '<li>Loading...</li>'

    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <div style="display: flex; align-items: center;">
        <label name="user_email">
            User: <strong>${userIdHtml}</strong>
        </label>
        <button type="button" id="copy_email" style="background: none; border: none; cursor: pointer; padding: 0.25rem;" title="Copy email">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M16 1H4C2.9 1 2 1.9 2 3V17H4V3H16V1ZM19 5H8C6.9 5 6 5.9 6 7V21C6 22.1 6.9 23 8 23H19C20.1 23 21 22.1 21 21V7C21 5.9 20.1 5 19 5ZM19 21H8V7H19V21Z" fill="currentColor"/>
          </svg>
        </button>
      </div>
      <div style="display: flex; align-items: center; gap: 0.5rem; margin-bottom: 1rem;">
        <label name="wallet_address">
            Address: <strong>${addressHtml}</strong>
        </label>
        <button type="button" id="copy_address" style="background: none; border: none; cursor: pointer; padding: 0.25rem;" title="Copy address">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path d="M16 1H4C2.9 1 2 1.9 2 3V17H4V3H16V1ZM19 5H8C6.9 5 6 5.9 6 7V21C6 22.1 6.9 23 8 23H19C20.1 23 21 22.1 21 21V7C21 5.9 20.1 5 19 5ZM19 21H8V7H19V21Z" fill="currentColor"/>
          </svg>
        </button>
      </div>
      <label name="balance_label">
          Wallet balance: 
          <ul>
              ${balanceHtml}
          </ul>
      </label>
      <form action="#" method="POST">
        <fieldset>
          <label id="address_type" hidden>
            Type of address 
            <select name="recipient" aria-label="Select the type of address you want to send ADA to" required id="type_selector">
              <option value="${AddressType.Bech32}">Bech32</option>
              <option value="${AddressType.Email}" selected="selected">Gmail</option>
            </select>
          </label>
          <label>
            Recipient's address
            <input
              name="zkfold_address" 
              id="address_input" 
              placeholder="example@gmail.com"
              required
            />
          </label>
          <label id="asset_name" hidden>
            Asset name ('lovelace' for ADA or '{PolicyID}.{AssetName}' for other assets) 
            <input
              name="zkfold_asset" 
              value="lovelace"
            />
          </label>
          <label>
            Amount 
            <input
              name="zkfold_amount"
              type="number"
              min="0.000001"
              step="0.000001"
              placeholder="Enter amount in ADA"
              style="-moz-appearance: textfield;"
              required
            />
          </label>
        </fieldset>
        <button type="submit">Send</button>
      </form>
      <label id="faucet_label" hidden>
        Use this address to receive funds from the <a href='https://docs.cardano.org/cardano-testnets/tools/faucet'>Faucet</a>: ${address}
      </label>
      <button id="show_selector">Show all controls</button>
      <button id="logout_button">Log out</button>
    `

    // Add copy functionality
    setTimeout(() => {
      const copyEmailBtn = document.getElementById('copy_email')
      const copyAddressBtn = document.getElementById('copy_address')

      if (copyEmailBtn) {
        copyEmailBtn.addEventListener('click', () => {
          this.copyToClipboard(userIdHtml, 'Email copied!', copyEmailBtn)
        })
      }

      if (copyAddressBtn) {
        copyAddressBtn.addEventListener('click', () => {
          this.copyToClipboard(address, 'Address copied!', copyAddressBtn)
        })
      }
    }, 0)

    return container
  }

  private async copyToClipboard(text: string, successMessage: string, buttonElement?: HTMLElement): Promise<void> {
    try {
      await navigator.clipboard.writeText(text)
      this.showCopyNotification(successMessage, buttonElement)
    } catch (err) {
      console.error('Failed to copy text: ', err)
      // Fallback for older browsers
      this.fallbackCopyTextToClipboard(text, successMessage, buttonElement)
    }
  }

  private fallbackCopyTextToClipboard(text: string, successMessage: string, buttonElement?: HTMLElement): void {
    const textArea = document.createElement("textarea")
    textArea.value = text
    textArea.style.top = "0"
    textArea.style.left = "0"
    textArea.style.position = "fixed"
    textArea.style.opacity = "0"

    document.body.appendChild(textArea)
    textArea.focus()
    textArea.select()

    try {
      const successful = document.execCommand('copy')
      if (successful) {
        this.showCopyNotification(successMessage, buttonElement)
      } else {
        this.showCopyNotification('Failed to copy text', buttonElement)
      }
    } catch (err) {
      console.error('Fallback: Oops, unable to copy', err)
      this.showCopyNotification('Failed to copy text', buttonElement)
    }

    document.body.removeChild(textArea)
  }

  private showCopyNotification(message: string, buttonElement?: HTMLElement): void {
    // Create a temporary notification
    const notification = document.createElement('div')
    notification.textContent = message

    let positionStyle = `
      position: fixed;
      top: 20px;
      right: 20px;
      background: #4CAF50;
      color: white;
      padding: 8px 12px;
      border-radius: 4px;
      z-index: 1000;
      font-size: 14px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      white-space: nowrap;
    `

    // If button element is provided, position the notification relative to it
    if (buttonElement) {
      const rect = buttonElement.getBoundingClientRect()
      positionStyle = `
        position: fixed;
        top: ${rect.top + window.scrollY}px;
        left: ${rect.right + 10}px;
        background: #4CAF50;
        color: white;
        padding: 8px 12px;
        border-radius: 4px;
        z-index: 1000;
        font-size: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        white-space: nowrap;
      `
    }

    notification.style.cssText = positionStyle

    document.body.appendChild(notification)

    // Remove notification after 2 seconds
    setTimeout(() => {
      if (notification.parentNode) {
        notification.parentNode.removeChild(notification)
      }
    }, 2000)
  }

  public renderSuccessView(data: { txId: string, recipient: string, isProofComputing?: boolean }): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'

    const txId = data?.txId || 'Unknown'
    const recipient = data?.recipient || 'Unknown'
    const isProofComputing = data?.isProofComputing || false

    // Determine the initial message based on proof computation state
    const initialMessage = isProofComputing
      ? 'Computing zero-knowledge proof. It will take approximately 5 minutes (one time operation).'
      : 'Transaction pending. This page will refresh automatically when transaction succeeds.'

    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <h1 id="tx_status">${initialMessage}</h1>
      <label name="txid_label">
          Transaction id: ${txId}
      </label>
      <br>
      <button id="new_tx" disabled>Make another transaction</button>
      <button id="new_wallet" disabled>Log out</button>
    `

    // If not computing proof, start transaction status checking immediately
    if (!isProofComputing) {
      this.addTransactionStatusScript(container, txId, recipient)
    }

    return container
  }

  public renderFailedView(data: { reason: string }): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'

    const reason = data?.reason || 'Unknown error'

    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <h1>Transaction failed.</h1>
      <label name="balance_label">
          Reason: ${reason}
      </label>
      <button id="new_tx" disabled>Make another transaction</button>
      <button id="new_wallet" disabled>Log out</button>
    `

    return container
  }

  private addTransactionStatusScript(_container: HTMLElement, txId: string, recipient: string): void {
    // Instead of using a script tag, we'll create a proper async function that uses the BackendService
    this.startTransactionStatusChecking(txId, recipient)
  }

  private async startTransactionStatusChecking(txId: string, recipient: string): Promise<void> {
    if (!this.backendService) {
      console.error('BackendService not available for transaction status checking')
      this.updateTransactionStatus('failure', 'Backend service not configured')
      return
    }

    console.log('Starting transaction status checking:', txId, recipient)

    const sleep = (delay: number) => new Promise((resolve) => setTimeout(resolve, delay))

    try {
      let response = await this.backendService.checkTransactionStatus(txId, recipient)

      while (response.outcome !== "success" && response.outcome !== "failure") {
        console.log('Transaction status:', response)
        await sleep(10000) // Wait 10 seconds
        response = await this.backendService.checkTransactionStatus(txId, recipient)
      }

      this.updateTransactionStatus(response.outcome, response.reason)
    } catch (error) {
      console.error('Error checking transaction status:', error)
      this.updateTransactionStatus('failure', 'Error checking transaction status')
    }
  }

  private updateTransactionStatus(outcome: string, reason?: string): void {
    const txStatus = document.getElementById("tx_status")
    const newTx = document.getElementById("new_tx") as HTMLButtonElement
    const newWallet = document.getElementById("new_wallet") as HTMLButtonElement

    if (!txStatus || !newTx || !newWallet) return

    if (outcome === "success") {
      txStatus.innerHTML = "Transaction successful!"
      newTx.disabled = false
      newWallet.disabled = false
    } else {
      txStatus.innerHTML = "Transaction failed: " + (reason || "Unknown error")
      newTx.disabled = false
      newWallet.disabled = false
    }
  }

  public updateProofComputationComplete(txId: string, recipient: string): void {
    const txStatus = document.getElementById("tx_status")
    const txIdLabel = document.querySelector('label[name="txid_label"]')

    if (txStatus) {
      txStatus.innerHTML = "Transaction pending. This page will refresh automatically when transaction succeeds."
      // Start transaction status checking now that proof is complete
      this.startTransactionStatusChecking(txId, recipient)
    }

    if (txIdLabel) {
      txIdLabel.innerHTML = `Transaction id: ${txId}`
    }
  }
}