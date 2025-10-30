import { Transaction, BigIntWrap } from "zkfold-smart-wallet-api"
import { getAddressLabel } from "./Address"
import { getAssetLabel, getAssetAmount } from "./Assets"
import * as CSL from '@emurgo/cardano-serialization-lib-browser';

export function formatTransactions(txList: Transaction[]): string {
  let transactions = ""
  for (const tx of txList) {   
    for (let [asset, value] of Object.entries(tx.value_diff)) {
        const txValue = `${getAssetLabel(asset)} ${getAssetAmount(asset, new BigIntWrap(value))}\n`
        const colour = value < 0 ? "text_red" : "text_green" 
        transactions +=
            `<li class="price_list_item">
              <button class="price_list_item_btn" type="button">
                <label class="price_label ${colour}">${txValue}</label>
                <label class="status completed">Completed</label>
              </button>
              <div class="price_list_item_value" style="display: none;">
                <p>${tx.timestamp}</p>
                <p>To: ${displayAddresses(tx.to_addrs)}</p>
              </div>
            </li>
            `
    }
  }
  if (transactions === "") {
    transactions = "<li>No transaction history</li>"
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
