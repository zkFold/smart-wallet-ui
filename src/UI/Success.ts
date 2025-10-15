import { Wallet } from "zkfold-smart-wallet-api"
import { renderAppHeader } from "./Header"

export function renderSuccessView(wallet: Wallet, data: { txId: string, recipient: string, isProofComputing?: boolean }): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container app-container'

  const txId = data?.txId || 'Unknown'
  const recipient = data?.recipient || 'Unknown'
  const isProofComputing = data?.isProofComputing || false

  // Determine the initial message based on proof computation state
  const message = isProofComputing
    ? 'Computing zero-knowledge proof. It will take approximately 5 minutes (one time operation).'
    : 'Transaction pending. This page will refresh automatically when transaction succeeds.'

  container.innerHTML = `
    <section class="app-shell status-shell">
      ${renderAppHeader()}
      <article class="info-card status-card pending" id="tx_card">
        <div class="card-header">
          <span class="card-title">Status</span>
        </div>
        <div class="card-body column">
          <p class="status-message" id="tx_status">${message}</p>
          <p class="status-detail" id="tx_reason" hidden></p>
          <div class="status-meta">
            <div class="status-meta-item">
              <span class="status-meta-label">Transaction id</span>
              <span class="status-meta-value">${txId}</span>
            </div>
            <div class="status-meta-item">
              <span class="status-meta-label">Recipient</span>
              <span class="status-meta-value">${recipient}</span>
            </div>
          </div>
        </div>
        <div class="status-actions">
          <button type="button" id="new_tx" disabled class="primary-action">Make another transaction</button>
          <button type="button" id="logout_button" disabled class="primary-action">Log out</button>
        </div>
      </article>
    </section>
  `

  // If not computing proof, start transaction status checking immediately
  if (!isProofComputing) {
    startTransactionStatusChecking(wallet, txId, recipient)
  }

  return container
}

async function startTransactionStatusChecking(wallet: Wallet, txId: string, recipient: string): Promise<void> {
  console.log('Starting transaction status checking:', txId, recipient)

  const sleep = (delay: number) => new Promise((resolve) => setTimeout(resolve, delay))

  try {
    let response = await wallet.checkTransactionStatus(txId, recipient)

    while (response.outcome !== "success" && response.outcome !== "failure") {
      console.log('Transaction status:', response)
      await sleep(10000) // Wait 10 seconds
      response = await wallet.checkTransactionStatus(txId, recipient)
    }

    updateTransactionStatus(response.outcome, response.reason)
  } catch (error) {
    console.error('Error checking transaction status:', error)
    updateTransactionStatus('failure', 'Error checking transaction status')
  }
}

function updateTransactionStatus(outcome: string, reason?: string): void {
  const txStatus = document.getElementById("tx_status")
  const newTx = document.getElementById("new_tx") as HTMLButtonElement
  const logoutButton = document.getElementById("logout_button") as HTMLButtonElement
  const statusCard = document.getElementById("tx_card")
  const reasonElement = document.getElementById("tx_reason")

  if (!txStatus || !newTx || !logoutButton) return

  if (statusCard) {
    statusCard.classList.remove("pending", "success", "failure")
  }

  if (outcome === "success") {
    txStatus.textContent = "Transaction successful!"
    if (statusCard) statusCard.classList.add("success")
    if (reasonElement) {
      reasonElement.textContent = "Your funds are on their way."
      reasonElement.hidden = false
    }
  } else {
    txStatus.textContent = "Transaction failed"
    if (statusCard) statusCard.classList.add("failure")
    if (reasonElement) {
      reasonElement.textContent = reason || "Unknown error"
      reasonElement.hidden = false
    }
  }

  newTx.disabled = false
  logoutButton.disabled = false
}