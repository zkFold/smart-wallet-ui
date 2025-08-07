import { AppView, WalletState } from '../Types'
import { EventEmitter } from '../Utils/EventEmitter'
import { formatBalance } from '../Utils/Helpers'
import { BackendService } from '../Services/BackendService'

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
      <div class="logo-container">
        <a href="https://zkfold.io">
          <img src="logo-200x73.png" alt="zkFold Logo">
        </a>
      </div>
      <div class="main-content">
        <div class="welcome-message">
          Create your wallet with email
        </div>
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
          <input
            id="submit"
            type="submit"
            value="Create Wallet"
            class="primary-button"
          />
        </form>
      </div>
    `

    return container
  }

  public renderWalletView(walletState: WalletState): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'

    const balanceHtml = walletState.balance ? formatBalance(walletState.balance) : '<li>Loading...</li>'
    const address = walletState.address || 'Loading...'
    const userEmail = walletState.userEmail || 'Unknown'

    container.innerHTML = `
      <div class="logo-container">
        <a href="https://zkfold.io">
          <img src="logo-200x73.png" alt="zkFold Logo">
        </a>
      </div>
      <div class="main-content">
        <div class="user-info">
          <strong>User:</strong> ${userEmail}
        </div>
        <div class="balance-container">
          <strong>Wallet Balance:</strong>
          <ul>
              ${balanceHtml}
          </ul>
        </div>
        <form action="#" method="POST">
          <fieldset>
            <label id="address_type" hidden>
              Type of address 
              <select name="recipient" aria-label="Select the type of address you want to send ADA to" required id="type_selector">
                <option>Bech32</option>
                <option selected="selected">Gmail</option>
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
                min="1"
                required
              />
            </label>
          </fieldset>
          <input
            type="submit"
            value="Send"
            class="primary-button"
          />
        </form>
        <label id="faucet_label" hidden>
          Use this address to receive funds from the <a href='https://docs.cardano.org/cardano-testnets/tools/faucet'>Faucet</a>: ${address}
        </label>
        <button class="outline secondary" id="show_address">Show address</button>
        <button class="outline secondary" id="show_selector">Show all controls</button>
        <button class="outline secondary" id="logout_button">Log out</button>
      </div>
    `

    return container
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
      <div class="logo-container">
        <a href="https://zkfold.io">
          <img src="logo-200x73.png" alt="zkFold Logo">
        </a>
      </div>
      <div class="main-content">
        <div class="status-container">
          <h1 id="tx_status">${initialMessage}</h1>
          <div class="transaction-info">
            <strong>Transaction ID:</strong> ${txId}
          </div>
          <button class="outline secondary" id="new_tx" disabled>Make another transaction</button>
          <button class="outline secondary" id="new_wallet" disabled>Initialise a new wallet</button>
        </div>
      </div>
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
      <div class="logo-container">
        <a href="https://zkfold.io">
          <img src="logo-200x73.png" alt="zkFold Logo">
        </a>
      </div>
      <div class="main-content">
        <div class="status-container">
          <h1>Transaction failed.</h1>
          <div class="transaction-info">
            <strong>Reason:</strong> ${reason}
          </div>
          <button class="outline secondary" id="new_tx" disabled>Make another transaction</button>
          <button class="outline secondary" id="new_wallet" disabled>Initialise a new wallet</button>
        </div>
      </div>
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
      newTx.onclick = () => this.emit('refreshAndNavigate', 'wallet')
      newWallet.onclick = () => this.navigate('init')
    } else {
      txStatus.innerHTML = "Transaction failed: " + (reason || "Unknown error")
      newTx.disabled = false
      newWallet.disabled = false
      newTx.onclick = () => this.emit('refreshAndNavigate', 'wallet')
      newWallet.onclick = () => this.navigate('init')
    }
  }

  public updateProofComputationComplete(txId: string, recipient: string): void {
    const txStatus = document.getElementById("tx_status")
    const txInfo = document.querySelector('.transaction-info')

    if (txStatus) {
      txStatus.innerHTML = "Transaction pending. This page will refresh automatically when transaction succeeds."
      // Start transaction status checking now that proof is complete
      this.startTransactionStatusChecking(txId, recipient)
    }

    if (txInfo) {
      txInfo.innerHTML = `<strong>Transaction ID:</strong> ${txId}`
    }
  }
}