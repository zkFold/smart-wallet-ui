import { BigIntWrap, Value } from "zkfold-smart-wallet-api"

// This module handles how assets are displayed in the UI

const DEFAULT_ASSET = 'lovelace'

export function getAssetLabel(assetKey: string): string {
  if (assetKey === DEFAULT_ASSET) {
    return 'ADA'
  }

  return assetKey.length > 12 ? assetKey.slice(0, 8) + '...' + assetKey.slice(-4) : assetKey
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

export function formatBalance(balance: Value): string {
  let assets = ""
  for (const [key, value] of Object.entries(balance)) {   
    let v: string
    v = getAssetAmount(key, value)

    assets +=
        `<li class="price_list_item">
          <label class="price_label">${getAssetLabel(key)}</label>
          <label class="price_label price_label_quentity">${v}</label>
        </li>
        `
  }
  if (assets === "") {
    assets = "<li>No assets available</li>"
  }
  return assets
}

export function formatAssetOptions(balance: Value): string {
  let options = ""
  for (const key of Object.keys(balance)) {
    options += `<option value="${key}">${getAssetLabel(key)}</option>\n`
  }
  return options
}