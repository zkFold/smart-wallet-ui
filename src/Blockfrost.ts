import { BlockFrostAPI } from '@blockfrost/blockfrost-js';
import { Provider } from './Provider';
import * as dotenv from 'dotenv'
dotenv.config()


export class BlockfrostProvider extends Provider {
    constructor() {
        super();
        this.API = new BlockFrostAPI({
                          projectId: process.env.BLOCKFROST_KEY 
                        });
    }

    async getUtxos(addr: string): Promise<UTxO[]> {
        return await this.API.addressesUtxos(addr)
    }

    getNetworkId(): number {
        return 1;
    }

}
