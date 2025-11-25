import { getAddressLabel } from "../Utils/Address"
import { formatBalance, formatAssetOptions, buildAssetMetadata, AssetMetadataMap } from "../Utils/Assets"
import { formatTransactions } from "../Utils/Transactions"
import { renderAppFrame } from "./Frame"
import { BalanceResponse, Transaction } from "zkfold-smart-wallet-api"

export function renderWalletView(userId: string, address: string, balance: BalanceResponse, transactions: Transaction[], isWalletActivated: boolean, assetMetadata?: AssetMetadataMap): HTMLElement {
  const addressHtml = getAddressLabel(address)
  const metadata = assetMetadata ?? buildAssetMetadata(balance)
  const balanceHtml = formatBalance(balance)
  const hasAssets = balance.lovelace > 0 || balance.tokens.length > 0
  const assetOptionsHtml = formatAssetOptions(balance)
  const transactionsListHtml = formatTransactions(transactions, metadata)
  const hasTransactions = transactionsListHtml.trim().length > 0

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
      <h3 class="wallet_summary_amount text_center">$${balance.usd.toFixed(2)}</h3>
      <div class="wallet_assets${hasAssets ? '' : ' empty'}">
        <ul id="wallet_assets_list" class="wallet_detail_list">
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
    <button type="button" class="wallet_btn toggle_btn">
          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-chevron-down h-4 w-4 transition-transform" data-lov-id="src/pages/Dashboard.tsx:126:22" data-lov-name="ChevronDown" data-component-path="src/pages/Dashboard.tsx" data-component-line="126" data-component-file="Dashboard.tsx" data-component-name="ChevronDown" data-component-content="%7B%7D"><path d="m6 9 6 6 6-6"></path></svg>
        </button>
      </div>
      <div class="wallet_transactions${hasTransactions ? '' : ' empty'}">
        <ul id="wallet_transactions_list" class="wallet_detail_list wallet_detail_list--transactions">
          ${transactionsListHtml}
        </ul>
  <div id="wallet_empty_transactions" class="empty_transactions">No transaction history yet.</div>
      </div>
    </div>
  `

  const activatedFees = `
        <li class="wallet_detail_list__item">
          <label class="wallet_detail_list__label">Cardano network fee</label>
          <label class="wallet_detail_list__label wallet_detail_list__value">0.3</label>
        </li>
  `

  const activationFees = `
        <li class="wallet_detail_list__item">
          <label class="wallet_detail_list__label">Cardano network fee</label>
          <label class="wallet_detail_list__label wallet_detail_list__value">1.7</label>
        </li>
        <li class="wallet_detail_list__item">
          <label class="wallet_detail_list__label">Wallet activation fee</label>
          <label class="wallet_detail_list__label wallet_detail_list__value">3.0</label>
        </li>
        <li class="wallet_detail_list__item">
          <label class="wallet_detail_list__label">Refundable deposit</label>
          <label class="wallet_detail_list__label wallet_detail_list__value">2.0</label>
        </li>
  `

  const feesHtml = `
    <div class="wallet_box">
      <div class="wallet_box_header">
        <label class="form_label">Total fees:   ${isWalletActivated ? '0.3 ADA' : '4.7 ADA'}</label>
        <button type="button" class="wallet_btn toggle_btn">
          <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-chevron-down h-4 w-4 transition-transform" data-lov-id="src/pages/Dashboard.tsx:126:22" data-lov-name="ChevronDown" data-component-path="src/pages/Dashboard.tsx" data-component-line="126" data-component-file="Dashboard.tsx" data-component-name="ChevronDown" data-component-content="%7B%7D"><path d="m6 9 6 6 6-6"></path></svg>
        </button>
      </div>
      <div class="transaction_fees">
        <ul id="transaction_fees_list" class="transaction_fees_list">
          ${isWalletActivated ? activatedFees : activationFees}
        </ul>
      </div>
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
    ${feesHtml}
  `

  return renderAppFrame(content, true)
}
