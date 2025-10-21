import { WalletBalance } from "../Types"
import { formatBalance, getVisibleAssetKeys, getAssetLabel } from "../Utils/Assets"
import { renderAppHeader } from "./Header"

export function renderWalletView(userId: string, address: string, balance: WalletBalance): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container app-container wallet-container'

  const visibleAssetKeys = getVisibleAssetKeys(balance)
  const assetOptionsHtml = visibleAssetKeys
    .map((assetKey, index) => {
      const selectedAttribute = index === 0 ? ' selected' : ''
      return `<option value="${assetKey}"${selectedAttribute}>${getAssetLabel(assetKey)}</option>`
    })
    .join('')

  container.innerHTML = `
    <section class="app-shell wallet-shell">
      ${renderAppHeader()}
      <div class="wallet-grid app-grid">
        <article class="info-card user-card">
          <div class="card-header">
            <span class="card-title">User</span>
          </div>
          <div class="card-body">
            <span class="card-value" data-testid="wallet-user">${userId}</span>
            <button type="button" id="copy_email" class="icon-button" title="Copy email">
              <svg width="20" height="20" viewBox="0 0 24 24" fill="none" aria-hidden="true" xmlns="http://www.w3.org/2000/svg">
                <path d="M16 1H4C2.9 1 2 1.9 2 3V17H4V3H16V1ZM19 5H8C6.9 5 6 5.9 6 7V21C6 22.1 6.9 23 8 23H19C20.1 23 21 22.1 21 21V7C21 5.9 20.1 5 19 5ZM19 21H8V7H19V21Z" fill="currentColor"/>
              </svg>
            </button>
          </div>
        </article>
        <article class="info-card address-card">
          <div class="card-header">
            <span class="card-title">Top up address</span>
          </div>
          <div class="card-body">
            <span class="card-value monospace" data-testid="wallet-address">${address}</span>
            <button type="button" id="copy_address" class="icon-button" title="Copy address">
              <svg width="20" height="20" viewBox="0 0 24 24" fill="none" aria-hidden="true" xmlns="http://www.w3.org/2000/svg">
                <path d="M16 1H4C2.9 1 2 1.9 2 3V17H4V3H16V1ZM19 5H8C6.9 5 6 5.9 6 7V21C6 22.1 6.9 23 8 23H19C20.1 23 21 22.1 21 21V7C21 5.9 20.1 5 19 5ZM19 21H8V7H19V21Z" fill="currentColor"/>
              </svg>
            </button>
          </div>
        </article>
        <article class="info-card balance-card">
          <div class="card-header">
            <span class="card-title">Wallet balance</span>
          </div>
          <div class="card-body balance-body">
            <ul class="balance-list">
              ${formatBalance(balance)}
            </ul>
          </div>
        </article>
      </div>
      <section class="info-card send-card">
        <div class="card-header">
          <span class="card-title">Send funds</span>
        </div>
        <form action="#" method="POST" class="send-form">
          <fieldset class="form-grid form-grid--send">
            <label class="form-control form-control--send-to">
              Send to
              <input
                name="zkfold_address"
                id="address_input"
                placeholder="friend@gmail.com or addr1xyz..."
                required
              />
            </label>
            <label class="form-control">
              Amount
              <input
                name="zkfold_amount"
                type="number"
                min="0.000001"
                step="0.000001"
                placeholder="Enter amount"
                required
              />
            </label>
            <label class="form-control">
              Asset
              <select
                name="zkfold_asset"
                id="asset_select"
                required
              >
                ${assetOptionsHtml}
              </select>
            </label>
          </fieldset>
          <div class="form-actions">
            <button type="submit" class="primary-action">Send</button>
            <button type="button" id="logout_button" class="primary-action">Log out</button>
          </div>
        </form>
      </section>
    </section>
  `
  // Add copy functionality
  setTimeout(() => {
    const copyEmailBtn = document.getElementById('copy_email')
    const copyAddressBtn = document.getElementById('copy_address')

    if (copyEmailBtn) {
      copyEmailBtn.addEventListener('click', () => {
        copyToClipboard(userId, 'Email copied!', copyEmailBtn)
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
    background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
    color: white;
    padding: 8px 14px;
    border-radius: 999px;
    z-index: 1000;
    font-size: 14px;
    box-shadow: 0 16px 40px -28px rgba(99, 102, 241, 0.75);
    white-space: nowrap;
  `

  // If button element is provided, position the notification relative to it
  if (buttonElement) {
    const rect = buttonElement.getBoundingClientRect()
    positionStyle = `
      position: fixed;
      top: ${rect.top + window.scrollY}px;
      left: ${rect.right + 10}px;
      background: linear-gradient(135deg, #6366f1 0%, #8b5cf6 100%);
      color: white;
      padding: 6px 12px;
      border-radius: 999px;
      z-index: 1000;
      font-size: 12px;
      box-shadow: 0 16px 40px -28px rgba(99, 102, 241, 0.75);
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