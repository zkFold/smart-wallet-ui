// Policy ID for Smart Wallet utility tokens that should be hidden
const HIDDEN_POLICY_ID = '31afd09d5c4ba63898e726f345cfa1c6fe33e2fe07f9845288ef8c4d'

export function formatBalance(balance: { [asset: string]: bigint }): string {
  let assets = ""
  for (const [key, value] of Object.entries(balance)) {
    // Skip tokens with the Smart Wallet utility policy ID
    if (key.startsWith(HIDDEN_POLICY_ID)) {
      continue
    }
    
    if (key === 'lovelace') {
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
