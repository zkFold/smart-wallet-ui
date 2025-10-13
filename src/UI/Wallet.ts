import { WalletBalance } from "../Types"
import { formatBalance } from "../Utils/Helpers"
import { AddressType } from "zkfold-smart-wallet-api"

export function renderWalletView(userId: string, address: string, balance: WalletBalance): HTMLElement {
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
        copyToClipboard(userIdHtml, 'Email copied!', copyEmailBtn)
      })
    }

    if (copyAddressBtn) {
      copyAddressBtn.addEventListener('click', () => {
        copyToClipboard(address, 'Address copied!', copyAddressBtn)
      })
    }
  }, 0)

  return container
}

async function copyToClipboard(text: string, successMessage: string, buttonElement?: HTMLElement): Promise<void> {
  try {
    await navigator.clipboard.writeText(text)
    showCopyNotification(successMessage, buttonElement)
  } catch (err) {
    console.error('Failed to copy text: ', err)
    // Fallback for older browsers
    fallbackCopyTextToClipboard(text, successMessage, buttonElement)
  }
}

function fallbackCopyTextToClipboard(text: string, successMessage: string, buttonElement?: HTMLElement): void {
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
      showCopyNotification(successMessage, buttonElement)
    } else {
      showCopyNotification('Failed to copy text', buttonElement)
    }
  } catch (err) {
    console.error('Fallback: Oops, unable to copy', err)
    showCopyNotification('Failed to copy text', buttonElement)
  }

  document.body.removeChild(textArea)
}

function showCopyNotification(message: string, buttonElement?: HTMLElement): void {
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