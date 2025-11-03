import { BalanceResponse } from "zkfold-smart-wallet-api"

// This module handles how assets are displayed in the UI

const DEFAULT_ASSET = 'lovelace'

export interface AssetDisplayMetadata {
  label: string
  decimals: number
}

export type AssetMetadataMap = Record<string, AssetDisplayMetadata>

export function buildAssetMetadata(balance: BalanceResponse): AssetMetadataMap {
  const metadata: AssetMetadataMap = {
    [DEFAULT_ASSET]: {
      label: 'ADA',
      decimals: 6
    }
  }

  for (const token of balance.tokens) {
    if (!token.ticker) {
      continue
    }

    metadata[token.asset] = {
      label: token.ticker,
      decimals: token.decimal_adjustment ?? 0
    }
  }

  return metadata
}

export function formatWithDecimals(amount: number, decimals: number): string {
  if (!decimals) {
    return amount.toString()
  }

  const divisor = 10 ** decimals
  return (amount / divisor).toFixed(decimals)
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
          <label class="price_label">${token.ticker}</label>
          <label class="price_label price_label_quentity">${formatWithDecimals(token.amount, token.decimal_adjustment ?? 0)}</label>
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
    options += `<option value="${token.asset}">${token.ticker}</option>\n`
  }
  return options
}
