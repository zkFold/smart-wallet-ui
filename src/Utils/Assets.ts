// This module handles how assets are displayed in the UI

const DEFAULT_ASSET = 'lovelace'

export function getAssetLabel(assetKey: string): string {
  if (assetKey === DEFAULT_ASSET) {
    return 'ADA'
  }

  return assetKey
}

export function formatBalance(balance: { [asset: string]: bigint }): string {
  let assets = ""
  for (const [key, value] of Object.entries(balance)) {   
    let v: string
    if (key === DEFAULT_ASSET) {
      // Convert lovelaces to ADA (1 ADA = 1,000,000 lovelaces)
      v = (Number(value) / 1_000_000).toFixed(6)
    }
    else {
      // For other assets, just show the raw amount
      v = value.toString()
    }

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

export function formatAssetOptions(balance: { [asset: string]: bigint }): string {
  let options = ""
  for (const key of Object.keys(balance)) {
    options += `<option value="${key}">${getAssetLabel(key)}</option>\n`
  }
  return options
}