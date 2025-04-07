import { BlockFrostAPI } from '@blockfrost/blockfrost-js';
import { Provider } from './Provider';
import * as dotenv from 'dotenv'
dotenv.config()

export class BlockFrostProvider extends Provider {
    constructor(network: string) {
        super();
        this.network = network;
        this.API = new BlockFrostAPI({
          projectId: process.env.BLOCKFROST_KEY, 
          network: network,
        });
    }

    async getUtxos(addr: string): Promise<UTxO[]> {
        return await this.API.addressesUtxos(addr); 
    }

    async submitTx(tx: string): Promise<string> {
        return await this.API.txSubmit(tx);
    }

    getNetworkId(): number {
        if (this.network == 'mainnet') {
            return 1;
        }
        return 0;
    }

    async getLatestParams() {
        return await this.API.epochsLatestParameters();
    }

    async txStatus(txId: string) {
        return await this.API.txs(txId);
    }

}
