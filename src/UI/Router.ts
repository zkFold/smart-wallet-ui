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

  public renderWalletView(walletState: WalletState): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'

    const balanceHtml = walletState.balance ? formatBalance(walletState.balance) : '<li>Loading...</li>'
    const address = walletState.address || 'Loading...'
    const userEmail = walletState.userEmail || 'Unknown'

    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <label name="user_email">
          User: <strong>${userEmail}</strong>
      </label>
      <br>
      <label name="wallet_address">
          Address: <strong>${address}</strong>
      </label>
      <br><br>
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
        />
      </form>
      <label id="faucet_label" hidden>
        Use this address to receive funds from the <a href='https://docs.cardano.org/cardano-testnets/tools/faucet'>Faucet</a>: ${address}
      </label>
      <button class="outline secondary" id="show_address">Show address</button>
      <button class="outline secondary" id="show_selector">Show all controls</button>
      <button class="outline" id="logout_button">Log out</button>
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
      <button id="new_wallet" disabled>Initialise a new wallet</button>
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
      <button id="new_wallet" disabled>Initialise a new wallet</button>
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