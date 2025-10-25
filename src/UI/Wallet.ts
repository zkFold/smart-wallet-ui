import { getAddressLabel } from "../Utils/Address"
import { formatBalance, formatAssetOptions } from "../Utils/Assets"
import { renderAppFrame } from "./Frame"
import { Value } from "zkfold-smart-wallet-api"

export function renderWalletView(userId: string, address: string, balance: Value): HTMLElement {
  const addressHtml = getAddressLabel(address)
  const balanceHtml = formatBalance(balance)
  const hasAssets = Object.keys(balance).length > 0
  const assetOptionsHtml = formatAssetOptions(balance)

  const userHtml = `
    <div class="wallet_box">
      <label class="form_label text_center">User</label>
      <div class="copy_cont">
        <p id="user_email">${userId}</p>
        <button type="button" id="copy_email" class="wallet_btn">
          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-copy h-4 w-4" data-lov-id="src/pages/Dashboard.tsx:105:18" data-lov-name="Copy" data-component-path="src/pages/Dashboard.tsx" data-component-line="105" data-component-file="Dashboard.tsx" data-component-name="Copy" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><rect width="14" height="14" x="8" y="8" rx="2" ry="2"></rect><path d="M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"></path></svg>
        </button>
      </div>
    </div>
  `

  const topupAddressHtml = `
    <div class="wallet_box">
      <label class="form_label text_center">Top up address</label>
      <div class="copy_cont">
        <p id="user_topup_adress">${addressHtml}</p>
        <button type="button" id="copy_topup_adress" class="wallet_btn">
          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-copy h-4 w-4" data-lov-id="src/pages/Dashboard.tsx:105:18" data-lov-name="Copy" data-component-path="src/pages/Dashboard.tsx" data-component-line="105" data-component-file="Dashboard.tsx" data-component-name="Copy" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><rect width="14" height="14" x="8" y="8" rx="2" ry="2"></rect><path d="M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"></path></svg>
        </button>
      </div>
    </div>
  `

  const walletBalanceHtml = `
    <div class="wallet_box">
      <div class="wallet_box_header">
        <label class="form_label">Wallet Balance</label>
        <button type="button" class="wallet_btn toggle_btn">
          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-chevron-down h-4 w-4 transition-transform" data-lov-id="src/pages/Dashboard.tsx:126:22" data-lov-name="ChevronDown" data-component-path="src/pages/Dashboard.tsx" data-component-line="126" data-component-file="Dashboard.tsx" data-component-name="ChevronDown" data-component-content="%7B%7D"><path d="m6 9 6 6 6-6"></path></svg>
        </button>
      </div>
      <h3 class="price text_center">$0.00</h3>
      <div class="wallet_assets${hasAssets ? '' : ' empty'}">
        <ul id="wallet_assets_list" class="price_list">
          ${hasAssets ? balanceHtml : ''}
        </ul>
        <div id="wallet_empty_assets" class="empty_assets">No assets yet. Top up your wallet to get started.</div>
      </div>
    </div>
  `

  const transactionsHtml = `
    <div class="wallet_box">
      <div class="wallet_box_header">
        <label class="form_label">Transactions</label>
        <button type="button"class="wallet_btn toggle_btn">
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
  `

  const sendToHtml = `
    <div class="form_fields">
      <div class="form_field_cont">
        <label for="sendTo">Send to</label>
        <input type="text" name="zkfold_address" class="input_field" placeholder="friend@gmail.com or addr1abc...xyz" autocomplete="off" required>
      </div>
      <div class="form_fields_row">
        <div class="col_1">
          <div class="form_field_cont">
            <label for="amount">Amount</label>
            <input type="number" name="zkfold_amount" class="input_field" placeholder="0.00" autocomplete="off" required>
          </div>
        </div>
        <div class="col_2">
          <div class="form_field_cont">
            <label for="sendTo">Asset</label>
            <select id="sendto_asset_select" name="zkfold_asset" class="input_field">
              <button>
                <selectedcontent></selectedcontent>
              </button>
              ${assetOptionsHtml}
            </select>
          </div>
        </div>
      </div>
      <button type="submit" name="submit" class="submit_btn">Send</button>
    </div>
  `

  const content = `
    ${userHtml}
    ${topupAddressHtml}
    ${walletBalanceHtml}
    ${transactionsHtml}
    ${sendToHtml}
  `

  return renderAppFrame(content, true)
}
