import { Transaction } from "zkfold-smart-wallet-api"
import { getAddressLabel } from "./Address"
import { AssetMetadataMap, formatWithDecimals } from "./Assets"
import * as CSL from '@emurgo/cardano-serialization-lib-browser';

export function formatTransactions(txList: Transaction[], assetMetadata: AssetMetadataMap = {}): string {
  let transactions = ""
  for (const tx of txList) {
    for (let [asset, value] of Object.entries(tx.value_diff)) {
        const metadata = assetMetadata[asset]
        if (!metadata) {
            continue
        }

        const formattedAmount = formatWithDecimals(value, metadata.decimals)
        const txValue = `${metadata.label} ${formattedAmount}\n`
  const isPositive = value > 0
  const colour = value < 0 ? "text_red" : "text_green"
  const addressLabel = isPositive ? "From" : "To"
  const counterpartAddresses = (isPositive ? tx.from_addrs : tx.to_addrs) ?? []
        transactions +=
            `<li class="wallet_detail_list__item">
              <button class="wallet_detail_list__item-btn" type="button">
                <label class="wallet_detail_list__label ${colour}">${txValue}</label>
                <label class="status completed">Completed</label>
              </button>
              <div class="wallet_detail_list__details" style="display: none;">
                <p>${tx.timestamp}</p>
                <p>${addressLabel}: ${displayAddresses(counterpartAddresses)}</p>
              </div>
            </li>
            `
    }
  }
  return transactions
}

function displayAddresses(addresses: CSL.Address[]): string {
    if (addresses.length === 0) {
        return ""
    }
    if (addresses.length === 1) {
        return `${getAddressLabel(addresses[0].to_bech32())}`
    }
    let addrs = ""
    for (const addr of addresses) {
        addrs += `${getAddressLabel(addr.to_bech32())}\n`
    }
    return addrs
}
