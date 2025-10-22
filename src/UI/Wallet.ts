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
    <section class="wallet_sec">
      <div class="wallet_container">
        <form class="wallet_form">
          <div class="form_header">
            <div class="form_col">
              <img class="form_logo" src="assets/img/zkfold-logo.png" alt="zkfold">
            </div>
            <div class="form_col btn_col">
              <button class="wallet_btn exit_btn">
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-log-out h-4 w-4" data-lov-id="src/pages/Dashboard.tsx:88:16" data-lov-name="LogOut" data-component-path="src/pages/Dashboard.tsx" data-component-line="88" data-component-file="Dashboard.tsx" data-component-name="LogOut" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"></path><polyline points="16 17 21 12 16 7"></polyline><line x1="21" x2="9" y1="12" y2="12"></line></svg>
              </button>
            </div>
          </div>
          <div class="wallet_box_cont">
            <div class="wallet_box">
              <label class="form_label text_center">User</label>
              <div class="copy_cont">
                <p id="user_email">${userId}</p>
                <button id="copy_email" class="wallet_btn copy_btn">
                  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-copy h-4 w-4" data-lov-id="src/pages/Dashboard.tsx:105:18" data-lov-name="Copy" data-component-path="src/pages/Dashboard.tsx" data-component-line="105" data-component-file="Dashboard.tsx" data-component-name="Copy" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><rect width="14" height="14" x="8" y="8" rx="2" ry="2"></rect><path d="M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"></path></svg>
                </button>
              </div>
            </div>
            <div class="wallet_box">
              <label class="form_label text_center">Top up address</label>
              <div class="copy_cont">
                <p id="user_topup_adress">0x1234567890abcdef...</p>
                <button id="copy_topup_adress" class="wallet_btn copy_btn">
                  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-copy h-4 w-4" data-lov-id="src/pages/Dashboard.tsx:105:18" data-lov-name="Copy" data-component-path="src/pages/Dashboard.tsx" data-component-line="105" data-component-file="Dashboard.tsx" data-component-name="Copy" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><rect width="14" height="14" x="8" y="8" rx="2" ry="2"></rect><path d="M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"></path></svg>
                </button>
              </div>
            </div>
            <div class="wallet_box">
              <div class="wallet_box_header">
                <label class="form_label">Wallet Balance</label>
                <button class="wallet_btn toggle_btn">
                  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-chevron-down h-4 w-4 transition-transform" data-lov-id="src/pages/Dashboard.tsx:126:22" data-lov-name="ChevronDown" data-component-path="src/pages/Dashboard.tsx" data-component-line="126" data-component-file="Dashboard.tsx" data-component-name="ChevronDown" data-component-content="%7B%7D"><path d="m6 9 6 6 6-6"></path></svg>
                </button>
              </div>
              <h3 class="price text_center">$0.00</h3>
              <ul class="price_list">
                <li class="price_list_item">
                  <label class="price_label">ADA</label>
                  <label class="price_label price_label_quentity">1,250.45</label>
                </li>
                <li class="price_list_item">
                  <label class="price_label">SNEK</label>
                  <label class="price_label price_label_quentity">50,000</label>
                </li>
                <li class="price_list_item">
                  <label class="price_label">DJED</label>
                  <label class="price_label price_label_quentity">320.75</label>
                </li>
                <li class="price_list_item">
                  <label class="price_label">MIN</label>
                  <label class="price_label price_label_quentity">8,500.2</label>
                </li>
              </ul>
            </div>
            <div class="wallet_box">
              <div class="wallet_box_header">
                <label class="form_label">Transactions</label>
                <button class="wallet_btn toggle_btn">
                  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-chevron-down h-4 w-4 transition-transform" data-lov-id="src/pages/Dashboard.tsx:126:22" data-lov-name="ChevronDown" data-component-path="src/pages/Dashboard.tsx" data-component-line="126" data-component-file="Dashboard.tsx" data-component-name="ChevronDown" data-component-content="%7B%7D"><path d="m6 9 6 6 6-6"></path></svg>
                </button>
              </div>
              <ul class="price_list price_list_2">
                <li class="price_list_item">
                  <button class="price_list_item_btn">
                    <label class="price_label text_red">-10 ADA</label>
                    <label class="status completed">Completed</label>
                  </button>
                  <div class="price_list_item_value" style="display: none;">
                    <p>2025-10-14 14:32</p>
                    <p>To: addr1qxy...abc123</p>
                  </div>
                </li>
                <li class="price_list_item">
                  <button class="price_list_item_btn">
                    <label class="price_label text_green">+250 SNEK</label>
                    <label class="status">In process</label>
                  </button>
                  <div class="price_list_item_value" style="display: none;">
                    <p>2025-10-15 09:15</p>
                    <p>From: addr1wxz...def456</p>
                  </div>
                </li>
                <li class="price_list_item">
                  <button class="price_list_item_btn">
                    <label class="price_label text_green">+15.5 DJED</label>
                    <label class="status completed">Completed</label>
                  </button>
                  <div class="price_list_item_value" style="display: none;">
                    <p>2025-10-13 18:45</p>
                    <p>From: addr1pqr...ghi789</p>
                  </div>
                </li>
                <li class="price_list_item">
                  <button class="price_list_item_btn">
                    <label class="price_label text_red">-100 MIN</label>
                    <label class="status completed">Completed</label>
                  </button>
                  <div class="price_list_item_value" style="display: none;">
                    <p>2025-10-12 11:20</p>
                    <p>To: addr1rst...jkl012</p>
                  </div>
                </li>
                <li class="price_list_item">
                  <button class="price_list_item_btn">
                    <label class="price_label text_green">+50 ADA</label>
                    <label class="status">In process</label>
                  </button>
                  <div class="price_list_item_value" style="display: none;">
                    <p>2025-10-16 08:00</p>
                    <p>From: addr1uvw...mno345</p>
                  </div>
                </li>
                <li class="price_list_item">
                  <button class="price_list_item_btn">
                    <label class="price_label text_red">-25.75 DJED</label>
                    <label class="status completed">Completed</label>
                  </button>
                  <div class="price_list_item_value" style="display: none;">
                    <p>2025-10-11 16:30</p>
                    <p>To: addr1xyz...pqr678</p>
                  </div>
                </li>
                <li class="price_list_item">
                  <button class="price_list_item_btn">
                    <label class="price_label text_green">+300 SNEK</label>
                    <label class="status completed">Completed</label>
                  </button>
                  <div class="price_list_item_value" style="display: none;">
                    <p>2025-10-10 12:10</p>
                    <p>From: addr1abc...stu901</p>
                  </div>
                </li>
              </ul>
            </div>
          </div>
          <div class="form_fields">
            <div class="form_field_cont">
              <label for="sendTo">Send to</label>
              <input type="text" name="sendTo" id="sendTo" class="input_field" placeholder="friend@gmail.com or 0x123456...">
            </div>
            <div class="form_fields_row">
              <div class="col_1">
                <div class="form_field_cont">
                  <label for="amount">Amount</label>
                  <input type="number" name="amount" id="amount" class="input_field" placeholder="0.00">
                </div>
              </div>
              <div class="col_2">
                <div class="form_field_cont">
                  <label for="sendTo">Asset</label>
                  <select name="asset" id="asset" class="input_field select_field">
                    <option value="ADA">ADA</option>
                    <option value="SNEK">SNEK</option>
                    <option value="DJED">DJED</option>
                    <option value="MIN">MIN</option>
                  </select>
                </div>
              </div>
            </div>
            <input type="submit" name="submit" class="submit_btn" value="Send">
          </div>
        </form>
      </div>
    </section>
    <div id="notification" class="notice_box">
      <h3 class="notice_head">Copied!</h3>
      <p id="notification_body" class="notice_body">Email copied to clipboard</p>
      <input id="notification_timeout_id" type="hidden">
      <svg id="notification_close_icon" class="close_icon" xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-x h-4 w-4" data-lov-id="src/components/ui/toast.tsx:76:4" data-lov-name="X" data-component-path="src/components/ui/toast.tsx" data-component-line="76" data-component-file="toast.tsx" data-component-name="X" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><path d="M18 6 6 18"></path><path d="m6 6 12 12"></path></svg>
    </div>
  `
  // Add copy functionality
  setTimeout(() => {
    const copyEmailBtn = document.getElementById('copy_email')
    if (copyEmailBtn) {
      copyEmailBtn.addEventListener('click', () => {
        copyToClipboard(userId, 'Email copied to clipboard')
      })
    }

    const copyTopupAddressBtn = document.getElementById('copy_topup_adress')
    if (copyTopupAddressBtn) {
      copyTopupAddressBtn.addEventListener('click', () => {
        copyToClipboard(address, 'Address copied to clipboard')
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

  return container
}

async function copyToClipboard(text: string, message: string): Promise<void> {
  await navigator.clipboard.writeText(text)

  const notification = document.getElementById('notification')
  const notificationBody = document.getElementById('notification_body')
  const notificationTimeoutId = document.getElementById('notification_timeout_id') as HTMLInputElement

  if (notification && notificationBody && notificationTimeoutId) {
    // Clear any existing timeout
    const existingTimeoutId = notificationTimeoutId.value
    clearTimeout(existingTimeoutId)

    // Update message
    notificationBody.textContent = message

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