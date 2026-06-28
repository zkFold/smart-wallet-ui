import { getAddressLabel } from '../Utils/Address'
import { formatBalance, formatAssetOptions, buildAssetMetadata } from '../Utils/Assets'
import { formatTransactions } from '../Utils/Transactions'
import { WalletViewModel } from '../Types'
import { renderAppFrame } from './Frame'
import type { BalanceResponse } from 'zkfold-smart-wallet-api'

export function renderWalletView(model: WalletViewModel): HTMLElement {
  const l1Metadata = buildAssetMetadata(model.l1Balance)
  const l2Metadata = buildAssetMetadata(model.l2Balance)
  const l1TransactionsHtml = formatTransactions(model.l1Transactions, l1Metadata)
  const l2TransactionsHtml = formatTransactions(model.l2Transactions, l2Metadata)

  const content = `
    ${renderAddressBox('L1 address', 'l1_address', 'copy_l1_address', model.l1Address)}
    ${renderAddressBox('L2 address', 'l2_address', 'copy_l2_address', model.l2Address)}
    ${renderBalanceBox('L1 assets', 'l1', model.l1Balance)}
    ${renderBalanceBox('L2 assets', 'l2', model.l2Balance)}
    ${renderBridgeInForm(model.l1Balance)}
    ${renderL2TransferForm(model.l2Balance)}
    ${renderBridgeOutForm(model.l2Balance)}
    ${renderActions(model)}
    ${renderTransactionsBox('L1 transactions', 'l1_transactions', l1TransactionsHtml)}
    ${renderTransactionsBox('L2 transactions', 'l2_transactions', l2TransactionsHtml)}
  `

  return renderAppFrame(content, true)
}

function renderAddressBox(label: string, addressId: string, copyId: string, address: string): string {
  return `
    <div class="wallet_box">
      <label class="form_label text_center">${label}</label>
      <div class="copy_cont">
        <p id="${addressId}" class="wallet_address_text" title="${escapeHtml(address)}">${escapeHtml(getAddressLabel(address))}</p>
        <button type="button" id="${copyId}" class="wallet_btn" title="Copy">
          ${copyIcon()}
        </button>
      </div>
    </div>
  `
}

function renderBalanceBox(label: string, prefix: string, balance: BalanceResponse): string {
  const balanceHtml = formatBalance(balance)
  const hasAssets = balance.lovelace > 0 || balance.tokens.length > 0

  return `
    <div class="wallet_box active">
      <div class="wallet_box_header">
        <label class="form_label">${label}</label>
        <button type="button" class="wallet_btn toggle_btn" title="Toggle">
          ${chevronIcon()}
        </button>
      </div>
      <h3 class="wallet_summary_amount text_center">${formatAda(balance.lovelace)} ADA</h3>
      <div class="wallet_assets${hasAssets ? '' : ' empty'}">
        <ul id="${prefix}_assets_list" class="wallet_detail_list">
          ${hasAssets ? balanceHtml : ''}
        </ul>
        <div class="empty_assets">No assets.</div>
      </div>
    </div>
  `
}

function renderBridgeInForm(balance: BalanceResponse): string {
  return renderActionForm({
    id: 'bridge_in_form',
    title: 'Bridge in',
    amountName: 'bridge_in_amount',
    assetName: 'bridge_in_asset',
    assetOptions: formatAssetOptions(balance),
    buttonText: 'Bridge in'
  })
}

function renderL2TransferForm(balance: BalanceResponse): string {
  return renderActionForm({
    id: 'l2_transfer_form',
    title: 'Send on L2',
    recipientName: 'l2_transfer_recipient',
    recipientLabel: 'L2 recipient',
    recipientPlaceholder: 'l2_...',
    amountName: 'l2_transfer_amount',
    assetName: 'l2_transfer_asset',
    assetOptions: formatAssetOptions(balance),
    buttonText: 'Send on L2'
  })
}

function renderBridgeOutForm(balance: BalanceResponse): string {
  return renderActionForm({
    id: 'bridge_out_form',
    title: 'Bridge out',
    recipientName: 'bridge_out_recipient',
    recipientLabel: 'L1 recipient',
    recipientPlaceholder: 'addr...',
    amountName: 'bridge_out_amount',
    assetName: 'bridge_out_asset',
    assetOptions: formatAssetOptions(balance),
    buttonText: 'Bridge out'
  })
}

interface ActionFormOptions {
  id: string
  title: string
  recipientName?: string
  recipientLabel?: string
  recipientPlaceholder?: string
  amountName: string
  assetName: string
  assetOptions: string
  buttonText: string
}

function renderActionForm(options: ActionFormOptions): string {
  const recipientHtml = options.recipientName
    ? `
      <div class="form_field_cont">
        <label for="${options.recipientName}">${options.recipientLabel}</label>
        <input
          id="${options.recipientName}"
          type="text"
          name="${options.recipientName}"
          class="input_field"
          placeholder="${options.recipientPlaceholder ?? ''}"
          autocomplete="off"
          required
        >
      </div>
    `
    : ''

  return `
    <form id="${options.id}" class="wallet_box wallet_action_form">
      <label class="form_label">${options.title}</label>
      ${recipientHtml}
      <div class="form_fields_row">
        <div class="col_1">
          <div class="form_field_cont">
            <label for="${options.amountName}">Amount</label>
            <input
              id="${options.amountName}"
              type="number"
              name="${options.amountName}"
              class="input_field"
              min="0"
              step="0.000001"
              autocomplete="off"
              required
            >
          </div>
        </div>
        <div class="col_2">
          <div class="form_field_cont">
            <label for="${options.assetName}">Asset</label>
            <select id="${options.assetName}" name="${options.assetName}" class="input_field">
              ${options.assetOptions}
            </select>
          </div>
        </div>
      </div>
      <button type="submit" class="submit_btn">${options.buttonText}</button>
    </form>
  `
}

function renderActions(model: WalletViewModel): string {
  const actionsHtml = model.actions.map((action) => {
    const statusClass = action.status === 'success'
      ? 'completed'
      : action.status === 'error'
        ? 'failed'
        : ''
    const timestamp = new Date(action.timestamp).toLocaleString()

    return `
      <li class="wallet_detail_list__item">
        <button class="wallet_detail_list__item-btn" type="button">
          <label class="wallet_detail_list__label">${escapeHtml(action.label)}</label>
          <label class="status ${statusClass}">${escapeHtml(action.status)}</label>
        </button>
        <div class="wallet_detail_list__details" style="display: none;">
          <p>${escapeHtml(timestamp)}</p>
          <p>${escapeHtml(action.detail)}</p>
        </div>
      </li>
    `
  }).join('')

  return `
    <div class="wallet_box active">
      <div class="wallet_box_header">
        <label class="form_label">Activity</label>
        <button type="button" class="wallet_btn toggle_btn" title="Toggle">
          ${chevronIcon()}
        </button>
      </div>
      <div class="wallet_transactions${actionsHtml ? '' : ' empty'}">
        <ul id="wallet_actions_list" class="wallet_detail_list wallet_detail_list--transactions">
          ${actionsHtml}
        </ul>
        <div class="empty_transactions">No activity.</div>
      </div>
    </div>
  `
}

function renderTransactionsBox(label: string, id: string, transactionsHtml: string): string {
  const hasTransactions = transactionsHtml.trim().length > 0

  return `
    <div class="wallet_box">
      <div class="wallet_box_header">
        <label class="form_label">${label}</label>
        <button type="button" class="wallet_btn toggle_btn" title="Toggle">
          ${chevronIcon()}
        </button>
      </div>
      <div class="wallet_transactions${hasTransactions ? '' : ' empty'}">
        <ul id="${id}_list" class="wallet_detail_list wallet_detail_list--transactions">
          ${transactionsHtml}
        </ul>
        <div class="empty_transactions">No transactions.</div>
      </div>
    </div>
  `
}

function formatAda(lovelace: number): string {
  return (lovelace / 1_000_000).toFixed(6)
}

function escapeHtml(value: string): string {
  return value
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;')
}

function copyIcon(): string {
  return `
    <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
      <rect width="14" height="14" x="8" y="8" rx="2" ry="2"></rect>
      <path d="M4 16c-1.1 0-2-.9-2-2V4c0-1.1.9-2 2-2h10c1.1 0 2 .9 2 2"></path>
    </svg>
  `
}

function chevronIcon(): string {
  return `
    <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
      <path d="m6 9 6 6 6-6"></path>
    </svg>
  `
}
