import { getAddressLabel } from '../Utils/Address'
import { formatBalance, formatAssetOptions, buildAssetMetadata, formatWithDecimals } from '../Utils/Assets'
import { formatTransactions } from '../Utils/Transactions'
import { BalanceEffect, DemoAction, WalletViewModel } from '../Types'
import { renderAppFrame } from './Frame'
import type { BalanceResponse } from 'zkfold-smart-wallet-api'

const FINAL_STATUSES = new Set(['confirmed', 'failed', 'submitting'])

export function renderWalletView(model: WalletViewModel): HTMLElement {
  const l1Metadata = buildAssetMetadata(model.l1Balance)
  const l2Metadata = buildAssetMetadata(model.l2Balance)
  const l1TransactionsHtml = formatTransactions(model.l1Transactions, l1Metadata)
  const l2TransactionsHtml = formatTransactions(model.l2Transactions, l2Metadata)
  const pendingEffects = activeBalanceEffects(model.actions)

  const content = `
    <div class="wallet_columns">
      <section class="wallet_column wallet_column--l1">
        <div class="wallet_column_header">
          <h2>L1</h2>
        </div>
        ${renderAddressBox('L1 address', 'l1_address', 'copy_l1_address', model.l1Address)}
        ${renderBalanceBox('L1 assets', 'l1', model.l1Balance, pendingEffects)}
        ${renderL1SendForm(model.l1SpendableBalance)}
        ${renderBridgeInForm(model.l1SpendableBalance)}
        ${renderTransactionsBox('L1 transactions', 'l1_transactions', l1TransactionsHtml)}
      </section>
      <section class="wallet_column wallet_column--l2">
        <div class="wallet_column_header">
          <h2>L2</h2>
        </div>
        ${renderAddressBox('L2 address', 'l2_address', 'copy_l2_address', model.l2Address)}
        ${renderBalanceBox('L2 assets', 'l2', model.l2Balance, pendingEffects)}
        ${renderL2TransferForm(model.l2SpendableBalance)}
        ${renderBridgeOutForm(model.l2SpendableBalance)}
        ${renderTransactionsBox('L2 transactions', 'l2_transactions', l2TransactionsHtml)}
      </section>
    </div>
    ${renderActions(model)}
  `

  return renderAppFrame(content, true)
}

function activeBalanceEffects(actions: DemoAction[]): BalanceEffect[] {
  return actions
    .filter((action) => !FINAL_STATUSES.has(action.status))
    .flatMap((action) => action.effects)
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

function renderBalanceBox(label: string, prefix: string, balance: BalanceResponse, pendingEffects: BalanceEffect[]): string {
  const balanceHtml = formatBalance(balance)
  const relevantEffects = pendingEffects.filter((effect) => effect.layer === prefix && effect.quantity !== 0)
  const hasAssets = balance.lovelace > 0 || balance.tokens.length > 0 || relevantEffects.length > 0
  const pendingHtml = relevantEffects.map(renderPendingEffect).join('')

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
          ${hasAssets ? balanceHtml + pendingHtml : ''}
        </ul>
        <div class="empty_assets">No assets.</div>
      </div>
    </div>
  `
}

function renderPendingEffect(effect: BalanceEffect): string {
  const sign = effect.quantity > 0 ? '+' : '-'
  const absQuantity = Math.abs(effect.quantity)
  const value = effect.asset === 'lovelace'
    ? `${sign}${formatAda(absQuantity)}`
    : `${sign}${formatWithDecimals(absQuantity, 0)}`
  const label = effect.asset === 'lovelace' ? 'ADA pending' : `${effect.asset} pending`

  return `
    <li class="wallet_detail_list__item wallet_detail_list__item--pending">
      <label class="wallet_detail_list__label">${escapeHtml(label)}</label>
      <label class="wallet_detail_list__label wallet_detail_list__value">${escapeHtml(value)}</label>
    </li>
  `
}

function renderL1SendForm(balance: BalanceResponse): string {
  return renderActionForm({
    id: 'l1_send_form',
    title: 'Send on L1',
    recipientName: 'l1_send_recipient',
    recipientLabel: 'L1 recipient',
    recipientPlaceholder: 'addr...',
    amountName: 'l1_send_amount',
    assetName: 'l1_send_asset',
    assetOptions: formatAssetOptions(balance),
    buttonText: 'Send on L1',
    disabled: !hasSpendableAssets(balance)
  })
}

function renderBridgeInForm(balance: BalanceResponse): string {
  return renderActionForm({
    id: 'bridge_in_form',
    title: 'Bridge in',
    amountName: 'bridge_in_amount',
    assetName: 'bridge_in_asset',
    assetOptions: formatAssetOptions(balance),
    buttonText: 'Bridge in',
    disabled: !hasSpendableAssets(balance)
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
    buttonText: 'Send on L2',
    disabled: !hasSpendableAssets(balance)
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
    buttonText: 'Bridge out',
    disabled: !hasSpendableAssets(balance)
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
  disabled?: boolean
}

function renderActionForm(options: ActionFormOptions): string {
  const disabled = options.disabled ? 'disabled' : ''
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
          ${disabled}
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
              ${disabled}
              required
            >
          </div>
        </div>
        <div class="col_2">
          <div class="form_field_cont">
            <label for="${options.assetName}">Asset</label>
            <select id="${options.assetName}" name="${options.assetName}" class="input_field" ${disabled}>
              ${options.assetOptions}
            </select>
          </div>
        </div>
      </div>
      <button type="submit" class="submit_btn" ${disabled}>${options.buttonText}</button>
    </form>
  `
}

function hasSpendableAssets(balance: BalanceResponse): boolean {
  return balance.lovelace > 0 || balance.tokens.some((token) => token.amount > 0)
}

function renderActions(model: WalletViewModel): string {
  const actionsHtml = model.actions.map((action) => {
    const statusClass = statusClassName(action.status)
    const timestamp = new Date(action.timestamp).toLocaleString()
    const txLine = action.txHash
      ? `<p>L2 tx: ${escapeHtml(shortHash(action.txHash))}</p>`
      : action.txId
        ? `<p>L1 tx: ${escapeHtml(shortHash(action.txId))}</p>`
        : ''

    return `
      <li class="wallet_detail_list__item">
        <button class="wallet_detail_list__item-btn" type="button">
          <label class="wallet_detail_list__label">${escapeHtml(action.label)}</label>
          <label class="status ${statusClass}">${escapeHtml(statusLabel(action.status))}</label>
        </button>
        <div class="wallet_detail_list__details" style="display: none;">
          <p>${escapeHtml(timestamp)}</p>
          <p>${escapeHtml(action.detail)}</p>
          ${txLine}
        </div>
      </li>
    `
  }).join('')

  return `
    <div class="wallet_box active wallet_activity_box">
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

function statusClassName(status: DemoAction['status']): string {
  if (status === 'confirmed') return 'completed'
  if (status === 'failed') return 'failed'
  if (status === 'batched') return 'batched'
  if (status === 'submitted' || status === 'queued') return 'queued'
  return ''
}

function statusLabel(status: DemoAction['status']): string {
  return status.charAt(0).toUpperCase() + status.slice(1)
}

function shortHash(hash: string): string {
  if (hash.length <= 18) {
    return hash
  }
  return `${hash.slice(0, 10)}...${hash.slice(-8)}`
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
