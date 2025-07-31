import { AppView, WalletState } from '../types'
import { EventEmitter } from '../utils/EventEmitter'
import { formatBalance } from '../utils/helpers'

export class Router extends EventEmitter {
  private currentViewData: any = null

  public navigate(view: AppView, data?: any): void {
    this.currentViewData = data
    this.emit('navigate', { view, data })
    
    // Update browser URL without reload
    const url = this.getUrlForView(view)
    window.history.pushState({ view, data }, '', url)
  }

  public getViewData(): any {
    return this.currentViewData
  }

  private getUrlForView(view: AppView): string {
    switch (view) {
      case 'wallet':
        return '/wallet'
      case 'success':
      case 'failed':
        return '/transaction'
      default:
        return '/'
    }
  }

  public renderInitView(): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'
    
    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <h1 id="header">Smart Wallet (Preprod)</h1>
      <form action="#" method="POST">
        <fieldset>
          <label id="network_selector" hidden>
            Network type
            <select name="network" id="network_option" aria-label="Select network" required>
              <option>Preview</option>
              <option selected="selected">Preprod</option>
            </select>
          </label>
          <label id="method_selector" hidden>
            Method 
            <select name="method" aria-label="Select creation method" id="method_options" required>
              <option>Mnemonic</option>
              <option selected="selected">Google Oauth</option>
            </select>
          </label>
          <input
            name="zkfold_method_data" 
            id="address_input" 
            placeholder="Mnemonic" 
            hidden 
            autocomplete="off"
          />
        </fieldset>
        <input
          id="submit"
          type="submit"
          value="Initialise wallet with Gmail"
        />
      </form>
      <button class="outline secondary" id="show_controls">Show advanced controls</button>
    `
    
    return container
  }

  public renderWalletView(walletState: WalletState): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'
    
    const balanceHtml = walletState.balance ? formatBalance(walletState.balance) : '<li>Loading...</li>'
    const address = walletState.address || 'Loading...'
    
    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <h1>Perform a transaction</h1>
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
    `
    
    return container
  }

  public renderSuccessView(data: { txId: string, recipient: string }): HTMLElement {
    const container = document.createElement('main')
    container.className = 'container'
    
    const txId = data?.txId || 'Unknown'
    const recipient = data?.recipient || 'Unknown'
    
    container.innerHTML = `
      <a href="https://zkfold.io">
        <img src="logo-200x73.png" style="width:250px;height:100px;">
      </a>
      <br><br>
      <h1 id="tx_status">Transaction pending. This page will refresh automatically when transaction succeeds.</h1>
      <label name="txid_label">
          Transaction id: ${txId}
      </label>
      <br>
      <button id="new_tx" disabled>Make another transaction</button>
      <button id="new_wallet" disabled>Initialise a new wallet</button>
    `
    
    // Add script to check transaction status
    this.addTransactionStatusScript(container, txId, recipient)
    
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
      <button onclick="window.location.href='/wallet'">Make another transaction</button>
      <button onclick="window.location.href='/'">Initialise a new wallet</button>
    `
    
    return container
  }

  private addTransactionStatusScript(container: HTMLElement, txId: string, recipient: string): void {
    // Create and inject the status checking script
    const script = document.createElement('script')
    script.textContent = `
      (async function checkStatus() {
        const sleep = (delay) => new Promise((resolve) => setTimeout(resolve, delay));
        const txId = "${txId}";
        const recipient = "${recipient}";
        const url = \`/tx_status?txId=\${txId}&recipient=\${recipient}\`;
        console.log('Checking transaction status:', url);

        try {
          let response = await fetch(url).then((resp) => resp.json());
          while (response.outcome !== "success" && response.outcome !== "failure") {
            console.log('Transaction status:', response);
            await sleep(10000);
            response = await fetch(url).then((resp) => resp.json());
          }
          
          const txStatus = document.getElementById("tx_status");
          const newTx = document.getElementById("new_tx");
          const newWallet = document.getElementById("new_wallet");
          
          if (response.outcome === "success") {
            txStatus.innerHTML = "Transaction successful!";
            newTx.disabled = false;
            newWallet.disabled = false;
            newTx.onclick = () => window.location.href = '/wallet';
            newWallet.onclick = () => window.location.href = '/';
          } else {
            txStatus.innerHTML = "Transaction failed: " + (response.reason || "Unknown error");
            newTx.disabled = false;
            newWallet.disabled = false;
            newTx.onclick = () => window.location.href = '/wallet';
            newWallet.onclick = () => window.location.href = '/';
          }
        } catch (error) {
          console.error('Error checking transaction status:', error);
          const txStatus = document.getElementById("tx_status");
          txStatus.innerHTML = "Error checking transaction status";
        }
      })();
    `
    
    container.appendChild(script)
  }
}