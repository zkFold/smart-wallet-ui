import CSL from '@emurgo/cardano-serialization-lib-nodejs';
import * as bip39 from '@scure/bip39';
import { wordlist } from '@scure/bip39/wordlists/english';
import { Provider } from './Provider';
import * as dotenv from 'dotenv'
import * as fs from 'fs';
import fs from 'fs-extra';
import { createRequire } from "module";
import { getJWT } from './GoogleToken'
const require = createRequire(import.meta.url);
var exec = require('child_process').execSync;

dotenv.config()


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

class CollateralPool {
    private rootKey: string;
    private accountKey: string;
    private utxoPubKey: string;
    
    constructor(mnemonic: string, network: string = 'mainnet') {
        this.network = network;
        const entropy = bip39.mnemonicToEntropy(mnemonic, wordlist);
        this.rootKey = CSL.Bip32PrivateKey.from_bip39_entropy(
              Buffer.from(entropy, 'hex'),
              Buffer.from(''),
            );
        this.accountKey = this.rootKey
          .derive(harden(1852)) // purpose
          .derive(harden(1815)) // coin type
          .derive(harden(0)); // account #0
        
        this.utxoPubKey = this.accountKey
          .derive(0) // external
          .derive(0)
          .to_public();
    }

    // Adapted from https://developers.cardano.org/docs/get-started/cardano-serialization-lib/generating-keys/
    getAddress(): CSL.Address {
        const paymentCred = CSL.Credential.from_keyhash(this.utxoPubKey.to_raw_key().hash()); 
        var netId;
        switch (this.network) {
            case "mainnet": {
                netId = CSL.NetworkInfo.mainnet().network_id();
                break;
            };
            case "preprod": {
                netId = CSL.NetworkInfo.testnet_preprod().network_id();
                break;
            };
            case "preview": {
                netId = CSL.NetworkInfo.testnet_preview().network_id();
                break;
            };
        };
        // cardano-serialization-lib does not support base addresses without staking credentials.
        // This is required when initialising the wallet with email
        // I'll create an Enterprise address instead for now.
        const baseAddr = CSL.EnterpriseAddress.new(
          netId,
          paymentCred,
        );
        
        return baseAddr.to_address()
    }

    getSkey() {
        return this.accountKey.derive(0).derive(0).to_raw_key();
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
            this.rootKey = CSL.Bip32PrivateKey.from_bip39_entropy(
                  Buffer.from(entropy, 'hex'),
                  Buffer.from(password),
                );
            this.deriveKeys();
        } else {
            // At this point, we assume that userId is a valid email accessible by the user (i.e. the user was able to complete Google authentication).
            const userId = initialiser.data;
            const contract = createWalletContract(userId);

            const plutusScriptBytes = Buffer.from(contract, 'hex'); 
            const plutusScript = CSL.PlutusScript.from_bytes_v3(plutusScriptBytes);

            this.walletScript = plutusScript;
            this.userId = userId;
            
            this.collateral_pool = new CollateralPool('faculty away cheap truck baby absorb guilt idle strategy merry toilet cotton arrow mix firm pact glimpse zoo celery marble parent library coffee hedgehog', this.network);
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

    private createAddress(paymentCred): CSL.Address {
        var netId;
        switch (this.network) {
            case "mainnet": {
                netId = CSL.NetworkInfo.mainnet().network_id();
                break;
            };
            case "preprod": {
                netId = CSL.NetworkInfo.testnet_preprod().network_id();
                break;
            };
            case "preview": {
                netId = CSL.NetworkInfo.testnet_preview().network_id();
                break;
            };
        };
        // cardano-serialization-lib does not support base addresses without staking credentials.
        // This is required when initialising the wallet with email
        // I'll create an Enterprise address instead for now.
        const baseAddr = CSL.EnterpriseAddress.new(
          netId,
          paymentCred,
        );
        
        return baseAddr.to_address()
    }

    // Adapted from https://developers.cardano.org/docs/get-started/cardano-serialization-lib/generating-keys/
    getAddress(): CSL.Address {
        const paymentCred = this.method == Method.Mnemonic 
                   ? CSL.Credential.from_keyhash(this.utxoPubKey.to_raw_key().hash()) 
                   : CSL.Credential.from_scripthash(this.walletScript.hash());
        return this.createAddress(paymentCred);
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
        var utxos;
        try {
            utxos = await this.provider.getUtxos(this.getAddress().to_bech32()); 
        } catch (err) {
            utxos = [];
        }
        return utxos;
    }

    async getCollateral(threshold: number = 5000000n): Promise<UTxO[]> {
        var utxos;
        if (this.method == Method.Mnemonic) {
            utxos = await this.getUtxos();
        } else {
            utxos = await this.provider.getUtxos(this.collateral_pool.getAddress().to_bech32());
        }
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

    async getUsedAddresses(): Primise<CSL.Address[]> {
        const utxos = await this.getUtxos();
        if (utxos === []) {
            return [];
        } else {
            return [this.getAddress()];
        }
    }
    
    async getUnusedAddresses(): Primise<CSL.Address[]> {
        const utxos = await this.getUtxos();
        if (utxos === []) {
            return [this.getAddress()];
        } else {
            return [];
        }
    }

    getRewardAddresses(): CSL.Address[] {
        return [];
    }

    getChangeAddress(): CSL.Address {
        return this.getAddress();
    }

    private async buildTx(senderAddress: CSL.Address, recipientAddress: CSL.Address, amountToSend: CSL.BigNum, utxos, collateral=[], redeemer: CSL.Redeemer=null): CSL.TransactionBuilder {
        const txBuilderCfg = 
            CSL.TransactionBuilderConfigBuilder.new()
            .fee_algo(
                CSL.LinearFee.new(
                CSL.BigNum.from_str("44"),
                CSL.BigNum.from_str("155381")
            )
            )
            .coins_per_utxo_byte(CSL.BigNum.from_str("4310"))
            .pool_deposit(CSL.BigNum.from_str("500000000"))
            .key_deposit(CSL.BigNum.from_str("2000000"))
            .max_value_size(5000)
            .max_tx_size(16384)
            .prefer_pure_change(true)
            .ex_unit_prices(CSL.ExUnitPrices.new(
               CSL.UnitInterval.new(
                 CSL.BigNum.from_str("577"),
                 CSL.BigNum.from_str("10000")
               ),
               CSL.UnitInterval.new(
                 CSL.BigNum.from_str("721"),
                 CSL.BigNum.from_str("10000000")
               )
             ))
            .build();
        
        const txBuilder = CSL.TransactionBuilder.new(txBuilderCfg);

        const txInputBuilder = CSL.TxInputsBuilder.new();

        utxos.forEach((utxo) => {
            const hash = CSL.TransactionHash.from_bytes(Buffer.from(utxo.tx_hash, "hex"))
            const input = CSL.TransactionInput.new(hash, utxo.tx_index);
            var ada = 0;
            for (let asset of utxo.amount) {
                if (asset.unit == 'lovelace') {
                    var quantity: number = +asset.quantity;
                    ada += quantity;
                }
            }
            const value = CSL.Value.new(CSL.BigNum.from_str(ada.toString()));
            const addr = CSL.Address.from_bech32(utxo.address);
            if (this.method == Method.Mnemonic) {
                txInputBuilder.add_regular_input(addr, input, value);
            } else {
                const witness = CSL.PlutusWitness.new_without_datum(this.walletScript, redeemer);
                txInputBuilder.add_plutus_script_input(witness, input, value);
            }
        });
        txBuilder.set_inputs(txInputBuilder);

        if (collateral != []) {
            const collateralBuilder = CSL.TxInputsBuilder.new();

            collateral.forEach((utxo) => {
                const hash = CSL.TransactionHash.from_bytes(Buffer.from(utxo.tx_hash, "hex"))
                const input = CSL.TransactionInput.new(hash, utxo.tx_index);
                var ada = 0;
                for (let asset of utxo.amount) {
                    if (asset.unit == 'lovelace') {
                        var quantity: number = +asset.quantity;
                        ada += quantity;
                    }
                }
                const value = CSL.Value.new(CSL.BigNum.from_str(ada.toString()));
                const addr = CSL.Address.from_bech32(utxo.address);
                collateralBuilder.add_regular_input(addr, input, value);
            });

            txBuilder.set_collateral(collateralBuilder);
        }

        const output = CSL.TransactionOutput.new(
                recipientAddress,
                CSL.Value.new(CSL.BigNum.from_str(amountToSend.toString())),
        );

        txBuilder.add_output(output);

        const ttl = getCardanoSlot() + 5 * 60; // 1 hr TODO maybe change this? 
        console.log(ttl);
        // TODO: what's the problem with TTL and script utxos?
        //txBuilder.set_ttl_bignum(CSL.BigNum.from_str(ttl.toString()));

        const epochParameters = await this.provider.getLatestParams();
        const costModels = epochParameters["cost_models_raw"];
        for (var key in costModels) {
            costModels[key] = costModels[key].map((x) => x.toString());
        }
        
        txBuilder.calc_script_data_hash(
          CSL.Costmdls.from_json(JSON.stringify(costModels)),
        );

        txBuilder.add_change_if_needed(senderAddress);
        
        return txBuilder;
    }

    async sendTo(rec: SmartTxRecipient): string {
        console.log(this.method);
        console.log(rec.recipientType);
        console.log(rec.address);
        console.log(rec.amount);


        const senderAddress = this.getAddress();
        var recipientAddress;

        if (rec.recipientType == AddressType.Gmail) {
            const contract = createWalletContract(rec.address); 
            const plutusScriptBytes = Buffer.from(contract, 'hex'); 
            const plutusScript = CSL.PlutusScript.from_bytes_v3(plutusScriptBytes);

            const paymentCred = CSL.Credential.from_scripthash(plutusScript.hash());
            recipientAddress = this.createAddress(paymentCred);
        } else {
            recipientAddress =  CSL.Address.from_bech32(rec.address); 
        }

        const amountToSend = rec.amount * 1000000;

        const utxos = await this.getUtxos();
        console.log(utxos);

        switch (this.method) {
            case Method.Mnemonic: {
                // A classical transaction from an address behind a private key to another address or a smart contract
                const txBuilder = await this.buildTx(senderAddress, recipientAddress, amountToSend, utxos, []);

                const txBody = txBuilder.build(); 

                const transaction = CSL.FixedTransaction.new_from_body_bytes(txBody.to_bytes());
                transaction.sign_and_add_vkey_signature(this.accountKey.derive(0).derive(0).to_raw_key());
                
                const signedTxHex = Buffer.from(transaction.to_bytes()).toString('hex');
                return await this.provider.submitTx(signedTxHex);
            };

            case Method.Google: {
                // A transaction from a Web2-initialised wallet to any kind of address
                
                const collateral = await this.getCollateral();
                console.log("COLLATERAL");
                console.log(collateral);

                const token = await getJWT();
                const jwt = token.id_token;
                const parts = jwt.split(".");
                console.log(parts);

                const header = JSON.parse(atob(parts[0].replace(/-/g, '+').replace(/_/g, '/')));
                const keyId = header.kid;
                const matchingKey = await getMatchingKey(keyId);
                const certificate = {
                    "kid": matchingKey.kid,
                    "e": matchingKey.e,
                    "n": matchingKey.n
                }

                const payload = JSON.parse(atob(parts[1].replace(/-/g, '+').replace(/_/g, '/')));
                const email = payload.email;

                if (email != this.userId) {
                    throw new Error("Account does not matches the one used to initialise the wallet.");
                }

                const proofData = { 
                    header: parts[0],
                    payload: parts[1], 
                    signature: parts[2],
                    certificate: JSON.stringify(certificate),
                    amount: amountToSend,
                    recipient: recipientAddress.to_bech32(),
                    input: "pi",
                    userId: this.userId,
                    pubkey: "pubkey"
                };

                const redeemerData = createRedeemer(proofData); 
                console.log("REDEEMER DATA START");
                console.log(redeemerData);
                console.log("REDEEMER DATA END");
                
                const redeemer = CSL.Redeemer.new(
                    CSL.RedeemerTag.new_spend(), 
                    CSL.BigNum.from_str("0"),
                    redeemerData,
                    CSL.ExUnits.new(CSL.BigNum.from_str("700000"), CSL.BigNum.from_str("300000000")) // TODO: Change these to appropriate values
                );

                const txBuilder = await this.buildTx(senderAddress, recipientAddress, amountToSend, utxos, collateral, redeemer);

                const tx = txBuilder.build_tx(); 
                const fixedTx = CSL.FixedTransaction.new(tx.body().to_bytes(), tx.witness_set().to_bytes(), tx.is_valid());
                fixedTx.sign_and_add_vkey_signature(this.collateral_pool.getSkey());

                const txHex = Buffer.from(fixedTx.to_bytes()).toString('hex');
                return await this.provider.submitTx(txHex);
            };
        };
    }

}

// Taken from https://forum.cardano.org/t/building-transaction-using-cardano-serialization-lib/126082
function getCardanoSlot() {
    const nowUnixTimestamp = Math.floor(new Date().getTime() / 1000); 
    const startShelleyUnixTimestamp = nowUnixTimestamp - 1596491091;
    console.log(nowUnixTimestamp);
    console.log(startShelleyUnixTimestamp);
    return startShelleyUnixTimestamp + 4492800;
}


function createWalletContract(userId: string) {
    const utf8Arr = new TextEncoder().encode(userId);
    const encoded = btoa(utf8Arr).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
    const scriptName = `${encoded}.plutus`;

    if (!fs.existsSync(scriptName)) {
        const createContractExe = process.env.CREATE_CONTRACT_EXE;
        const cmd = `${createContractExe}/smart-wallet-creator --create --id ${userId} --pubkey dummy --output ${process.cwd()}`;
        console.log(cmd);
        exec(cmd,
          function (error, stdout, stderr) {
              console.log('stdout: ' + stdout);
              console.log('stderr: ' + stderr);
              if (error !== null) {
                   console.log('exec error: ' + error);
              }
          });
          fs.renameSync('./smartWallet.plutus', `./${scriptName}`);
    }
    const contract = JSON.parse(fs.readFileSync(`./${scriptName}`, 'utf-8'));
    return contract.cborHex;
}

function createRedeemer(proofData) {
    const createContractExe = process.env.CREATE_CONTRACT_EXE;
    const cmd = `${createContractExe}/smart-wallet-creator --validate --header ${proofData.header} --payload ${proofData.payload} --signature ${proofData.signature} --certificate ${proofData.certificate} --amount ${proofData.amount} --recipient ${proofData.recipient} --input ${proofData.input} --id ${proofData.userId} --pubkey dummy --output ${process.cwd()}`;
    console.log(cmd);
    exec(cmd,
      function (error, stdout, stderr) {
          console.log('stdout: ' + stdout);
          console.log('stderr: ' + stderr);
          if (error !== null) {
               console.log('exec error: ' + error);
          }
      });
    const plutusData = fs.readFileSync('./proof.cbor');
    return CSL.PlutusData.from_bytes(plutusData); 
}

async function getMatchingKey(keyId: string) {
    const { keys } = await fetch('https://www.googleapis.com/oauth2/v3/certs').then((res) => res.json());
    for (let k of keys) {
        if (k.kid == keyId) {
                return k;
        }
    }
    return null;
}
