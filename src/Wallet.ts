import CardanoWasm from '@emurgo/cardano-serialization-lib-nodejs';
import * as bip39 from '@scure/bip39';
import { wordlist } from '@scure/bip39/wordlists/english';
import { Provider } from './Provider';

function harden(num: number): number {
  return 0x80000000 + num;
}

export enum Method {
    Mnemonic = 0;
    Google = 1;
}

export enum AddressType {
    Bech32 = 0;
    Gmail = 1;
}

export interface Initialiser {
    method: Method;
    data: string;
}

export class SmartTxRecipient {
    recipientType: AddressType;
    address: string;
    amount: BigNum;

    constructor(recipientType: AddressType, address: string, amount: BigNum) {
        this.recipientType = recipientType;
        this.address = address;
        this.amount = amount;
    }
}

export class Wallet {
    private rootKey: string;
    private accountKey: string;
    private utxoPubKey: string;
    private stakeKey: string;

    constructor(provider: Provider, name: string, initialiser: Initialiser, password: string = '', network: string = 'mainnet') {
        this.provider = provider;
        this.name = name;
        this.network = network;
        this.method = initialiser.method;

        if (this.method == Method.Mnemonic) {
            const entropy = bip39.mnemonicToEntropy(initialiser.data, wordlist);
            this.rootKey = CardanoWasm.Bip32PrivateKey.from_bip39_entropy(
                  Buffer.from(entropy, 'hex'),
                  Buffer.from(password),
                );
            this.deriveKeys();
        } else {
            // TODO: Create and save wallet script
            this.walletScript = "";
        }
    }

    getName(): string {
        return this.name;
    }

    // Adapted from https://developers.cardano.org/docs/get-started/cardano-serialization-lib/generating-keys/
    private deriveKeys(): void {
        this.accountKey = this.rootKey
          .derive(harden(1852)) // purpose
          .derive(harden(1815)) // coin type
          .derive(harden(0)); // account #0
        
        this.utxoPubKey = this.accountKey
          .derive(0) // external
          .derive(0)
          .to_public();
        
        this.stakeKey = this.accountKey
          .derive(2) // chimeric
          .derive(0)
          .to_public();
    }

    // Adapted from https://developers.cardano.org/docs/get-started/cardano-serialization-lib/generating-keys/
    getAddress(): CardanoWasm.Address {
        const paymentCred = this.method == Method.Mnemonic 
                   ? CardanoWasm.Credential.from_keyhash(this.utxoPubKey.to_raw_key().hash()) 
                   : ScriptHash.from_hex(this.walletScript);
        var netId;
        switch (this.network) {
            case "mainnet": {
                netId = CardanoWasm.NetworkInfo.mainnet().network_id();
                break;
            };
            case "preprod": {
                netId = CardanoWasm.NetworkInfo.testnet_preprod().network_id();
                break;
            };
            case "preview": {
                netId = CardanoWasm.NetworkInfo.testnet_preview().network_id();
                break;
            };
        };
        // cardano-serialization-lib does not support base addresses without staking credentials.
        // This is required when initialising the wallet with email
        // I'll create an Enterprise address instead for now.
        const baseAddr = CardanoWasm.EnterpriseAddress.new(
          netId,
          paymentCred,
        );
        
        return baseAddr.to_address()
    }

    async getBalance(): Promise<Asset> {
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
        var result = {};
        for (let unit in assets) {
            result[unit] = assets[unit];
        };

        return result;
    }

    getExtensions(): string[] {
        return [];
    }

    async getUtxos(): Promise<UTxO[]> {
        return await this.provider.getUtxos(this.getAddress().to_bech32());
    }

    async getCollateral(threshold: number = 5000000n): Promise<UTxO[]> {
        const utxos = await this.getUtxos();
        const adaOnly = utxos.filter((u) => u.amount.every((a) => a.unit === 'lovelace'));
        var ans = [];
        var sum = 0;
        for (let i = 0; i < adaOnly.length; i++) {
            ans.push(adaOnly[i]);
            sum += adaOnly[i].amount.map((a) => a.quantity).reduce((a, b) => a + b, 0);
            if (sum >= threshold) {
                return ans;
            };
        };
        return [];
    }

    async getUsedAddresses(): Primise<CardanoWasm.Address[]> {
        const utxos = await this.getUtxos();
        if (utxos === []) {
            return [];
        } else {
            return [this.getAddress()];
        }
    }
    
    async getUnusedAddresses(): Primise<CardanoWasm.Address[]> {
        const utxos = await this.getUtxos();
        if (utxos === []) {
            return [this.getAddress()];
        } else {
            return [];
        }
    }

    getRewardAddresses(): CardanoWasm.Address[] {
        return [];
    }

    getChangeAddress(): CardanoWasm.Address {
        return this.getAddress();
    }

    async sendTo(rec: SmartTxRecipient): string {
        console.log(this.method);
        console.log(rec.recipientType);
        console.log(rec.address);
        console.log(rec.amount);
        switch (this.method) {
            case Method.Mnemonic: {
                switch (rec.recipientType) {
                    // A classical transaction from an address behind a private key to an address behind a private key
                    case AddressType.Bech32: {
                        const senderAddress = this.getAddress();
                        const recipientAddress = CardanoWasm.Address.from_bech32(rec.address); 
                        const amountToSend = rec.amount * 1000000;

                        const txBuilderCfg = 
                            CardanoWasm.TransactionBuilderConfigBuilder.new()
                            .fee_algo(
                                CardanoWasm.LinearFee.new(
                                CardanoWasm.BigNum.from_str("44"),
                                CardanoWasm.BigNum.from_str("155381")
                            )
                            )
                            .coins_per_utxo_byte(CardanoWasm.BigNum.from_str("4310"))
                            .pool_deposit(CardanoWasm.BigNum.from_str("500000000"))
                            .key_deposit(CardanoWasm.BigNum.from_str("2000000"))
                            .max_value_size(5000)
                            .max_tx_size(16384)
                            .prefer_pure_change(true)
                            .build();
                        
                        const txBuilder = CardanoWasm.TransactionBuilder.new(txBuilderCfg);

                        const txInputBuilder = CardanoWasm.TxInputsBuilder.new();

                        const utxos = await this.getUtxos();
                        console.log(utxos);

                        utxos.forEach((utxo) => {
                            const hash = CardanoWasm.TransactionHash.from_bytes(Buffer.from(utxo.tx_hash, "hex"))
                            const input = CardanoWasm.TransactionInput.new(hash, utxo.tx_index);
                            var ada = 0;
                            for (let asset of utxo.amount) {
                                if (asset.unit == 'lovelace') {
                                    var quantity: number = +asset.quantity;
                                    ada += quantity;
                                }
                            }
                            const value = CardanoWasm.Value.new(CardanoWasm.BigNum.from_str(ada.toString()));
                            const addr = CardanoWasm.Address.from_bech32(utxo.address);
                            txInputBuilder.add_regular_input(addr, input, value);
                        });
                        txBuilder.set_inputs(txInputBuilder);

                        const output = CardanoWasm.TransactionOutput.new(
                                recipientAddress,
                                CardanoWasm.Value.new(CardanoWasm.BigNum.from_str(amountToSend.toString())),
                        );

                        txBuilder.add_output(output);

                        const ttl = getCardanoSlot() + 60 * 60; // 1 hr TODO maybe change this? 
                        txBuilder.set_ttl_bignum(CardanoWasm.BigNum.from_str(ttl.toString()));
                        
                        txBuilder.add_change_if_needed(senderAddress)

                        const txBody = txBuilder.build(); 

                        const transaction = CardanoWasm.FixedTransaction.new_from_body_bytes(txBody.to_bytes());
                        transaction.sign_and_add_vkey_signature(this.accountKey.derive(0).derive(0).to_raw_key());
                        
                        const signedTxHex = Buffer.from(transaction.to_bytes()).toString('hex');
                        console.log(signedTxHex);
                        return await this.provider.submitTx(signedTxHex);
                    };
                    // A transaction from a classical address to a smart wallet (send to email)
                    case AddressType.Gmail: {

                        break;
                    };
                };
                break;
            };

            case Method.Google: {
                switch (rec.recipientType) {
                    // A transaction from a Web2-initialised wallet to a classical address
                    case AddressType.Bech32: {

                        break;
                    };
                    // A transaction between smart wallets (send to email)
                    case AddressType.Gmail: {

                        break;
                    };
                };
                break;
            };
        };
    }

}

// Taken from https://forum.cardano.org/t/building-transaction-using-cardano-serialization-lib/126082
function getCardanoSlot() {
    const nowUnixTimestamp = new Date().getTime();
    const startShelleyUnixTimestamp = nowUnixTimestamp - 1596491091;
    return startShelleyUnixTimestamp + 4924800;
}
