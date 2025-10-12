import { WalletManager } from '../WalletManager'

export function renderSuccessView(wm: WalletManager, data: { txId: string, recipient: string, isProofComputing?: boolean }): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container'

  const txId = data?.txId || 'Unknown'
  const recipient = data?.recipient || 'Unknown'
  const isProofComputing = data?.isProofComputing || false

  // Determine the initial message based on proof computation state
  const message = isProofComputing
    ? 'Computing zero-knowledge proof. It will take approximately 5 minutes (one time operation).'
    : 'Transaction pending. This page will refresh automatically when transaction succeeds.'

  container.innerHTML = `
    <a href="https://zkfold.io">
      <img src="logo-200x73.png" style="width:250px;height:100px;">
    </a>
    <br><br>
    <h1 id="tx_status">${message}</h1>
    <label name="txid_label">
        Transaction id: ${txId}
    </label>
    <br>
    <button id="new_tx" disabled>Make another transaction</button>
    <button id="new_wallet" disabled>Log out</button>
  `

  // If not computing proof, start transaction status checking immediately
  if (!isProofComputing) {
    startTransactionStatusChecking(wm, txId, recipient)
  }

  return container
}

async function startTransactionStatusChecking(wm: WalletManager, txId: string, recipient: string): Promise<void> {
  console.log('Starting transaction status checking:', txId, recipient)

  const sleep = (delay: number) => new Promise((resolve) => setTimeout(resolve, delay))

  try {
    let response = await wm.checkTransactionStatus(txId, recipient)

    while (response.outcome !== "success" && response.outcome !== "failure") {
      console.log('Transaction status:', response)
      await sleep(10000) // Wait 10 seconds
      response = await wm.checkTransactionStatus(txId, recipient)
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