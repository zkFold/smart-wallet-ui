export * as cip30 from './src/cip30';
import { CIP30 } from './src/cip30';
import { BlockfrostProvider } from './src/Blockfrost' 
import { LucidProvider } from './src/Lucid' 
import { Wallet } from './src/Wallet'

const api = new CIP30(new LucidProvider())

const mnemonic = [ "test", "walk", "nut", "penalty", "hip", "pave", "soap", "entry", "language", "right", "filter", "choice" ].join(' ');

async function main() {
    const w = new Wallet('test', mnemonic)
    console.log(w.getAddresses().map((x) => x.to_bech32()));
    api.activate(w);
    const utxos = await api.getUtxos();
    console.log(utxos);
    const balance = await api.getBalance();
    console.log(balance);
}

main();
