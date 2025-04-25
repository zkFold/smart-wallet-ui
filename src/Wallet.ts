import CSL from '@emurgo/cardano-serialization-lib-nodejs';
import * as bip39 from '@scure/bip39';
import { wordlist } from '@scure/bip39/wordlists/english';
import { Backend, UTxO, ProofBytes, Output } from './Backend';

// TODO: Modifying global state is a big no-no, but how else can it be done?
// JS has no idea how to serialise BigNum.
BigInt.prototype.toJSON = function () {
  return JSON.rawJSON(this.toString()); 
};


function harden(num: number): number {
  return 0x80000000 + num;
}

export enum Method {
    Mnemonic = 0,
    Google = 1
}

export enum AddressType {
    Bech32 = 0,
    Gmail = 1
}

export interface Initialiser {
    method: Method;
    data: string;
}

export class SmartTxRecipient {
    recipientType: AddressType;
    address: string;
    amount: CSL.BigNum;

    constructor(recipientType: AddressType, address: string, amount: CSL.BigNum) {
        this.recipientType = recipientType;
        this.address = address;
        this.amount = amount;
    }
}

export class Wallet {
    private rootKey: CSL.Bip32PrivateKey;
    private accountKey: CSL.Bip32PrivateKey;
    private utxoPubKey: CSL.Bip32PublicKey;
    private stakeKey: CSL.Bip32PublicKey;
    private jwt: string;
    private userId: string;
    private backend: Backend; 
    private tokenSKey: CSL.Bip32PrivateKey;
    private method: Method;
    private network: string;

    constructor(backend: Backend, initialiser: Initialiser, password: string = '', network: string = 'mainnet') {
        this.backend = backend;
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
            this.jwt = initialiser.data; 

            const parts = this.jwt.split(".");
            const payload = JSON.parse(atob(parts[1].replace(/-/g, '+').replace(/_/g, '/')));
            this.userId = payload.email;

            const prvKey = CSL.Bip32PrivateKey
                  .generate_ed25519_bip32()
                  .derive(harden(1852)) // purpose
                  .derive(harden(1815)) // coin type
                  .derive(harden(0)) // account #0
                  .derive(0)
                  .derive(0);
            this.tokenSKey = prvKey;
        }
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

    async addressForGmail(gmail: string): Promise<CSL.Address> {
        return await this.backend.walletAddress(gmail);
    }

    // Adapted from https://developers.cardano.org/docs/get-started/cardano-serialization-lib/generating-keys/
    async getAddress(): Promise<CSL.Address> {
        switch (this.method) {
            case Method.Mnemonic: {
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
            };
            case Method.Google: {
                return await this.addressForGmail(this.userId);  
            };
        }
    }

    async getBalance(): Promise<Map<string, number>> {
        const utxos = await this.getUtxos();
        var assets = {};
        for (let i=0; i < utxos.length; i++) {
            for (const key in utxos[i].value) {
                if (!(key in assets)) {
                    assets[key] = 0;
                }
                assets[key] += utxos[i].value[key];
            }
        };
        return assets;
    }

    getExtensions(): string[] {
        return [];
    }

    async getUtxos(): Promise<UTxO[]> {
        const address = await this.getAddress();
        var utxos: UTxO[] = [];
        try {
            utxos = await this.backend.addressUtxo(address); 
        } catch (err) {
            utxos = [];
        }
        return utxos;
    }

    async getUsedAddresses(): Promise<CSL.Address[]> {
        const utxos = await this.getUtxos();
        const address = await this.getAddress();
        if (utxos.length == 0) {
            return [];
        } else {
            return [address];
        }
    }
    
    async getUnusedAddresses(): Promise<CSL.Address[]> {
        const utxos = await this.getUtxos();
        const address = await this.getAddress();
        if (utxos.length == 0) {
            return [address];
        } else {
            return [];
        }
    }

    async getRewardAddresses(): Promise<CSL.Address[]> {
        return [];
    }

    async getChangeAddress(): Promise<CSL.Address> {
        return await this.getAddress();
    }

    private async buildTx(senderAddress: CSL.Address, recipientAddress: CSL.Address, amountToSend: CSL.BigNum): Promise<CSL.TransactionBuilder> {
        const utxos = await this.getUtxos();
        console.log(utxos);

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
            if (utxo.value.get('lovelace') != null) {
                const ada = utxo.value.get('lovelace');
                const hash = CSL.TransactionHash.from_bytes(Buffer.from(utxo.ref.transaction_id, "hex"))
                const input = CSL.TransactionInput.new(hash, utxo.ref.output_index);
                const value = CSL.Value.new(CSL.BigNum.from_str(ada.toString()));
                const addr = utxo.address;
                txInputBuilder.add_regular_input(addr, input, value);
            }
        });
        txBuilder.set_inputs(txInputBuilder);

        const output = CSL.TransactionOutput.new(
                recipientAddress,
                CSL.Value.new(CSL.BigNum.from_str(amountToSend.toString())),
        );

        txBuilder.add_output(output);

        txBuilder.add_change_if_needed(senderAddress);
        
        return txBuilder;
    }

    async sendTo(rec: SmartTxRecipient): Promise<string> {
        console.log(this.method);
        console.log(rec.recipientType);
        console.log(rec.address);
        console.log(rec.amount);


        const senderAddress = await this.getAddress();
        var recipientAddress;

        if (rec.recipientType == AddressType.Gmail) {
            recipientAddress = await this.addressForGmail(rec.address);
        } else {
            recipientAddress = CSL.Address.from_bech32(rec.address); 
        }

        const amountToSend = rec.amount * 1000000;

        switch (this.method) {
            case Method.Mnemonic: {
                // A classical transaction from an address behind a private key to another address or a smart contract
                const txBuilder = await this.buildTx(senderAddress, recipientAddress, amountToSend);

                const txBody = txBuilder.build(); 

                const transaction = CSL.FixedTransaction.new_from_body_bytes(txBody.to_bytes());
                transaction.sign_and_add_vkey_signature(this.accountKey.derive(0).derive(0).to_raw_key());
                
                const signedTxHex = Buffer.from(transaction.to_bytes()).toString('hex');
                return await this.backend.submitTx(signedTxHex);
            };

            case Method.Google: {
                // A transaction from a Web2-initialised wallet to any kind of address
                const is_initialised = await this.backend.isWalletInitialised(this.userId);
                console.log(`Is initialised: ${is_initialised}`);
                var txHex;

                const outs: Output[] = [{address: recipientAddress.to_bech32(), value: { 'lovelace': amountToSend }}];

                if (is_initialised) {
                    const resp = await this.backend.sendFunds(this.userId, outs, this.tokenSKey.to_public().to_raw_key().hash().to_hex());
                    txHex = resp.transaction;
                } else {
                    const prvKey = CSL.Bip32PrivateKey
                          .generate_ed25519_bip32()
                          .derive(harden(1852)) // purpose
                          .derive(harden(1815)) // coin type
                          .derive(harden(0)) // account #0
                          .derive(0)
                          .derive(0);
                    this.tokenSKey = prvKey;
                    const pubkeyHex = prvKey.to_public().to_raw_key().hash().to_hex();
                    const parts = this.jwt.split(".");
                    const header  = atob(parts[0].replace(/-/g, '+').replace(/_/g, '/'));
                    const payload = atob(parts[1].replace(/-/g, '+').replace(/_/g, '/'));
                    const resp = await this.backend.createAndSendFunds(this.userId, header + '.' + payload, pubkeyHex, dummyProofBytes, outs);
                    txHex = resp.transaction;
                }
                const transaction = CSL.FixedTransaction.from_bytes(hexToBytes(txHex));
                transaction.sign_and_add_vkey_signature(this.tokenSKey.to_raw_key());
                const signedTxHex = Buffer.from(transaction.to_bytes()).toString('hex');

                return await this.backend.submitTx(signedTxHex);
            };
        };
    }

}

async function getMatchingKey(keyId: string): Promise<string | null> {
    const { keys } = await fetch('https://www.googleapis.com/oauth2/v3/certs').then((res) => res.json());
    for (let k of keys) {
        if (k.kid == keyId) {
                return k;
        }
    }
    return null;
}

// Convert a hex string to a byte array
// https://stackoverflow.com/questions/14603205/how-to-convert-hex-string-into-a-bytes-array-and-a-bytes-array-in-the-hex-strin
function hexToBytes(hex: string): Uint8Array {
    let bytes = [];
    for (let c = 0; c < hex.length; c += 2)
        bytes.push(parseInt(hex.substr(c, 2), 16));
    return Uint8Array.from(bytes);
}

const dummyProofBytes: ProofBytes = {
    "cmA_bytes": "393766316433613733313937643739343236393536333863346661396163306663333638386334663937373462393035613134653361336631373162616335383663353565383366663937613161656666623361663030616462323263366262",
    "cmB_bytes": "393766316433613733313937643739343236393536333863346661396163306663333638386334663937373462393035613134653361336631373162616335383663353565383366663937613161656666623361663030616462323263366262",
    "cmC_bytes": "623132333661356332663866323965333635663338343538666239353634653265613666383530343165666163663837303739613734353664383631333261643233313232313262636463613364373830656430393537396532383537343135",
    "cmF_bytes": "633030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030",
    "cmH1_bytes": "633030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030",
    "cmH2_bytes": "633030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030",
    "cmZ1_bytes": "613935316661346136366334386264623634366438376337313738336635646362386633616138313231393536346564383336393633333863336266633135366635353031643466323636613763393235386565343563303233313862646537",
    "cmZ2_bytes": "393766316433613733313937643739343236393536333863346661396163306663333638386334663937373462393035613134653361336631373162616335383663353565383366663937613161656666623361663030616462323263366262",
    "cmQlow_bytes": "623033346330653239383838636436383633333834356266633037376565636163326238636164623539663535303436303630393664306637643331383837323734373737643632303234363538343737303538626562373735366662656236",
    "cmQmid_bytes": "623462313730313934386435643265333665313838393533346365373534346133636331623736303461323465386464633636316233653936623161616239303638373466393461303436343761633563633232386561376563363638356136",
    "cmQhigh_bytes": "633030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030",
    "proof1_bytes": "623330386335373739363436313936383535306632386432333833396166373237323361333130383332363332613139653961623531326134613565636234633863383935306164653032353737323439353032323231333736396133663464",
    "proof2_bytes": "623365343262343835303035643963326261346136366466626234646566323433336439326137623166643734376566346165643462356635653934343037396264653739616431646135383035333539376234626538313934373233666331",
    "a_xi_int": 1n,
    "b_xi_int": 1n,
    "c_xi_int": 48380510586722627616411267202495116783057255243693228940120047704204371350546n,
    "s1_xi_int": 10368790864104277489349149849901642613910605061180645600781012523028298814297n,
    "s2_xi_int": 29133202091870269236732546656522855889661645594339077247007036693772071783753n,
    "f_xi_int": 0n,
    "t_xi_int": 0n,
    "t_xi'_int": 0n,
    "z1_xi'_int": 40497370593942275679614638124878515092846558874156949013549943373738078556493n,
    "z2_xi'_int": 1n,
    "h1_xi'_int": 0n,
    "h2_xi_int": 0n,
    "l1_xi": 37713268627753681891487380051493928725054683102581668523304176199511429320989n
};
