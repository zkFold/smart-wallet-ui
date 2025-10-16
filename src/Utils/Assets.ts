import { WalletBalance } from "@/Types"

// This module handles how assets are displayed in the UI

// Policy ID for Smart Wallet utility tokens that should be hidden
const HIDDEN_POLICY_ID = '31afd09d5c4ba63898e726f345cfa1c6fe33e2fe07f9845288ef8c4d'
const DEFAULT_ASSET = 'lovelace'

export function getVisibleAssetKeys(balance: WalletBalance): string[] {
  const visibleAssets = new Set<string>([DEFAULT_ASSET])

  for (const key of Object.keys(balance)) {
    if (key.startsWith(HIDDEN_POLICY_ID)) continue

    visibleAssets.add(key)
  }

  return Array.from(visibleAssets)
}

export function getAssetLabel(assetKey: string): string {
  if (assetKey === DEFAULT_ASSET) {
    return 'ADA'
  }

  return assetKey
}

export function formatBalance(balance: { [asset: string]: bigint }): string {
  let assets = ""
  for (const [key, value] of Object.entries(balance)) {
    // Skip tokens with the Smart Wallet utility policy ID
    if (key.startsWith(HIDDEN_POLICY_ID)) {
      continue
    }
    
    if (key === DEFAULT_ASSET) {
      // Convert lovelaces to ADA (1 ADA = 1,000,000 lovelaces)
      const adaValue = Number(value) / 1_000_000
      assets += `<li><b>${adaValue.toFixed(6)}</b> <i>ada</i></li>`
    } else {
      assets += `<li><b>${value}</b> <i>${key}</i></li>`
    }
  }
  if (assets === "") {
    assets = "<li>No assets available</li>"
  }
  return assets
}
