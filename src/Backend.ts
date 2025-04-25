import CSL from '@emurgo/cardano-serialization-lib-nodejs';
import { createRequire } from "module";
import axios from 'axios';

export interface ProofBytes {
    "a_xi_int": BigInt, 
    "b_xi_int": BigInt,
    "c_xi_int": BigInt,
    "cmA_bytes": string,
    "cmB_bytes": string,
    "cmC_bytes": string,
    "cmF_bytes": string,
    "cmH1_bytes": string,
    "cmH2_bytes": string,
    "cmQhigh_bytes": string,
    "cmQlow_bytes": string,
    "cmQmid_bytes": string,
    "cmZ1_bytes": string,
    "cmZ2_bytes": string,
    "f_xi_int": BigInt,
    "h1_xi'_int": BigInt,
    "h2_xi_int": BigInt,
    "l1_xi": BigInt,
    "proof1_bytes": string,
    "proof2_bytes": string,
    "s1_xi_int": BigInt,
    "s2_xi_int": BigInt,
    "t_xi'_int": BigInt,
    "t_xi_int": BigInt,
    "z1_xi'_int": BigInt,
    "z2_xi'_int": BigInt
}

export interface Output {
    address: string,
    datum?: string[],
    value: Map<string, number>
}

export interface Reference {
    transaction_id: string,
    output_index: number
}

export interface UTxO {
    ref: Reference,
    address: CSL.Address,
    value: Map<string, number>
}

export interface CreateWalletResponse {
    address: CSL.Address,
    transaction: string,
    transaction_fee: number,
    transaction_id: string
}

export interface SendFundsResponse {
    transaction: string,
    transaction_fee: number,
    transaction_id: string
}

export class Backend {
    private url: string;
    private secret: string;

    constructor(url: string, secret: string) {
        this.url = url;
        this.secret = secret;
    }

    async walletAddress(email: string): Promise<CSL.Address> {
        const { data } = await axios.post(`${this.url}/v0/wallet/address`, {
            'email': email
          }, {
            headers: {
              'api-key': this.secret
            }
          }
        );
        return CSL.Address.from_bech32(data.address);
    }

    async isWalletInitialised(email: string): Promise<boolean> {
        const { data } = await axios.post(`${this.url}/v0/wallet/is-initialized`, {
            'email': email
          }, {
            headers: {
              'api-key': this.secret
            }
          }
        );
        return data.is_initialised != null; 
    }

    async createWallet(email: string, jwt: string, payment_key_hash: string, proof_bytes: ProofBytes, fund_address?: CSL.Address): Promise<CreateWalletResponse> {
        const { data } = await axios.post(`${this.url}/v0/wallet/create`, {
            'email': email,
            'jwt': jwt,
            'payment_key_hash': payment_key_hash,
            'proof_bytes': proof_bytes,
            'fund_address': fund_address
          }, {
            headers: {
              'api-key': this.secret
            }
          }
        );

        const response: CreateWalletResponse = { 
            address: CSL.Address.from_bech32(data.address),
            transaction: data.transaction,
            transaction_fee: data.transaction_fee,
            transaction_id: data.transaction_id
        };

        return response;
    }

    async createAndSendFunds(email: string, jwt: String, payment_key_hash: string, proof_bytes: ProofBytes, outs: Output[]): Promise<CreateWalletResponse> {
        const { data } = await axios.post(`${this.url}/v0/wallet/create-and-send-funds`, {
            'email': email,
            'jwt': jwt,
            'payment_key_hash': payment_key_hash,
            'proof_bytes': proof_bytes,
            'outs': outs,
          }, {
            headers: {
              'api-key': this.secret
            }
          }
        );

        const response: CreateWalletResponse = { 
            address: CSL.Address.from_bech32(data.address),
            transaction: data.transaction,
            transaction_fee: data.transaction_fee,
            transaction_id: data.transaction_id
        };

        return response;
    }

    async sendFunds(email: string, outs: Output[], payment_key_hash: string): Promise<SendFundsResponse> {
        const { data } = await axios.post(`${this.url}/v0/wallet/send-funds`, {
            'email': email,
            'outs': outs,
            'payment_key_hash': payment_key_hash,
          }, {
            headers: {
              'api-key': this.secret
            }
          }
        );

        const response: SendFundsResponse = { 
            transaction: data.transaction,
            transaction_fee: data.transaction_fee,
            transaction_id: data.transaction_id
        };

        return response;
    }

    async submitTx(tx: string): Promise<string> {
        const { data } = await axios.post(`${this.url}/v0/tx/submit`, tx, {
            headers: {
              'api-key': this.secret,
              "Content-Type": "application/json"
            }
          }
        );
        
        return data;
    }

    async addressUtxo(address: CSL.Address): Promise<UTxO[]> {
        const { data } = await axios.post(`${this.url}/v0/utxo/addresses`, [address.to_bech32()], {
            headers: {
              'api-key': this.secret
            }
          }
        );
        
        let result: UTxO[] = [];

        for (let i=0; i<data.length; i++) {
            const ref = data[i].ref;
            const parts = ref.split("#");
            const reference: Reference = {
                transaction_id: parts[0],
                output_index: Number(parts[1])
            }
            const utxo: UTxO = {
                ref: reference,
                address: CSL.Address.from_bech32(data[i].address),
                value: data[i].value
            }
            result.push(utxo);
        }

        return result;
    }
}

