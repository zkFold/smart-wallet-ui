import { BackendService } from '../Services/BackendService'

export class Router {
  private backendService?: BackendService

  constructor(backendService?: BackendService) {
    this.backendService = backendService
  }

  public setBackendService(backendService: BackendService): void {
    this.backendService = backendService
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