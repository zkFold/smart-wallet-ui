import { BigIntWrap, Value, BalanceResponse } from "zkfold-smart-wallet-api"

// This module handles how assets are displayed in the UI

const DEFAULT_ASSET = 'lovelace'

export function getAssetLabel(assetKey: string): string {
  if (assetKey === DEFAULT_ASSET) {
    return 'ADA'
  }

  return assetKey.length > 8 ? assetKey.slice(0, 4) + '...' + assetKey.slice(-4) : assetKey
}

export function getAssetAmount(asset: string, amount: BigIntWrap): string {
  if (asset === DEFAULT_ASSET) {
      // Convert lovelaces to ADA (1 ADA = 1,000,000 lovelaces)
      return (Number(amount) / 1_000_000).toFixed(6)
    }
    else {
      // For other assets, just show the raw amount
      return amount.toString()
    }
}

export function formatBalance(balance: BalanceResponse): string {
  let assets = ""
  if (balance.lovelace > 0) {
    assets +=
        `<li class="price_list_item">
          <label class="price_label">ADA</label>
          <label class="price_label price_label_quentity">${balance.lovelace / 1_000_000}</label>
        </li>
        `
  }
  for (const token of balance.tokens) {   
    assets +=
        `<li class="price_list_item">
          <label class="price_label">${token.token_name}</label>
          <label class="price_label price_label_quentity">${token.amount}</label>
        </li>
        `
  }
  if (assets === "") {
    assets = "<li>No assets available</li>"
  }
  return assets
}

export function formatAssetOptions(balance: BalanceResponse): string {
  let options = ""
  if (balance.lovelace > 0) {
    options += `<option value="lovelace">ADA</option>\n`
  }
  for (const token of balance.tokens) {
    options += `<option value="${token.asset}">${token.token_name}</option>\n`
  }
  return options
}
