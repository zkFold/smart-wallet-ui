import { Provider } from './Provider';
import { UTxO, Asset } from './Types';
import { Wallet } from './Wallet';
import { C } from 'lucid-cardano';

export class CIP30 {
    constructor(provider: Provider) {
        this.provider = provider;
    }

    activate(wallet: Wallet): void {
        this.wallet = wallet;
    }

    getNetworkId(): number {
        return this.provider.getNetworkId();
    }

    getExtensions(): string[] {
        return [];
    }

    async getUtxos(): Promise<UTxO[]> {
        return await this.provider.getUtxos(this.wallet.getAddresses()[0].to_bech32());
    }

    async getCollateral(threshold: number): Promise<UTxO[]> {
        const utxos = await this.getUtxos();
        const adaOnly = utxos.filter((u) => u.amount.every((a) => a.unit === 'lovelace'));
        var ans = [];
        var sum = 0;
        var ix = 0;
        while (sum < threshold) {
            ans.push(adaOnly[ix]);
            ix++;
        }
        return ans;
    }

    async getBalance(): Promise<Asset[]> {
        const utxos = await this.getUtxos();
        var assets = {};
        for (let utxo of utxos) {
            for (let asset of utxo.amount) {
                if (!(asset.unit in assets)) {
                    assets[asset.unit] = 0;
                }
                var quantity: number = +asset.quantity;
                assets[asset.unit] += quantity;
            }
        };
        var result = [];
        for (let unit in assets) {
            result.push({ "unit": unit, "quantity": assets[unit] });
        };

        return result;
    }

    async getUsedAddresses(): Primise<Address[]> {
        const utxos = await this.getUtxos();
        if (utxos === []) {
            return [];
        } else {
            return [this.wallet.getAddresses()[0]];
        }
    }
    
    async getUnusedAddresses(): Primise<Address[]> {
        const utxos = await this.getUtxos();
        if (utxos === []) {
            return [this.wallet.getAddresses()[0]];
        } else {
            return [];
        }
    }

    getChangeAddresses(): Address[] {
        return [this.wallet.getChangeAddress()];
    }

    getRewardAddresses(): Address[] {
        return [this.wallet.getRewardAddress()];
    }

    async signTx(cbor: string) {
        const tx = C.Transaction.from_bytes(Buffer.from(cbor, 'hex'));
        const signedTx = await tx.sign().complete();
        return signedTx;
    }

    async signData(address: string, payload: string) {
        const signedMessage = await this.provider.backend().newMessage(address, payload).sign();
        return signedMessage;
    }

    submitTx(cbor: string, signature: string): string {
        const txVkeyWitnesses = C.TransactionWitnessSet.from_bytes(
          Buffer.from(signature, 'hex')
        );
        const pubKeyHash = txVkeyWitnesses
          .vkeys()
          ?.get(0)
          .vkey()
          .public_key()
          .hash()
          .to_hex();
    
        const tx = C.Transaction.from_bytes(Buffer.from(cbor, 'hex'));
        const txSignerPubKeyHash = tx.body().required_signers()?.get(0).to_hex();
    
        const isVerified = pubKeyHash?.length > 0 && pubKeyHash == txSignerPubKeyHash;
        if (isVerified) {
            tx.submit();
        }
    }

}
