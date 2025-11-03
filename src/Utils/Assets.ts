import { BigIntWrap, BalanceResponse, PrettyToken } from "zkfold-smart-wallet-api"

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

function formatTokenAmount(token: PrettyToken): string {
  const decimals = token.decimal_adjustment ?? 0
  if (!decimals) {
    return token.amount.toString()
  }

  const divisor = 10 ** decimals
  const adjusted = token.amount / divisor
  return adjusted.toFixed(decimals)
}

export function formatBalance(balance: BalanceResponse): string {
  let assets = ""
  if (balance.lovelace > 0) {
    assets +=
        `<li class="price_list_item">
          <label class="price_label">ADA</label>
          <label class="price_label price_label_quentity">${(balance.lovelace / 1_000_000).toFixed(6)}</label>
        </li>
        `
  }
  for (const token of balance.tokens) {
    if (!token.ticker) {
      continue
    }
    assets +=
        `<li class="price_list_item">
          <label class="price_label">${getAssetLabel(token.ticker)}</label>
          <label class="price_label price_label_quentity">${formatTokenAmount(token)}</label>
        </li>
        `
  }
  return assets
}

export function formatAssetOptions(balance: BalanceResponse): string {
  let options = `<option value="lovelace">ADA</option>\n`
  for (const token of balance.tokens) {
    if (!token.ticker) {
      continue
    }
    options += `<option value="${token.asset}">${getAssetLabel(token.ticker)}</option>\n`
  }
  return options
}
