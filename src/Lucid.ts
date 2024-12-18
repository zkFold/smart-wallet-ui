import { Lucid, Blockfrost } from "lucid-cardano"
import { Provider } from './Provider';
import * as dotenv from 'dotenv'
dotenv.config()

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-mainnet.blockfrost.io/api/v0",
    process.env.BLOCKFROST_KEY,
  ),
);

export class LucidProvider extends Provider {
    constructor() {
        super();
        this.API = lucid; 
    }

    async getUtxos(addr: string): Promise<UTxO[]> {
        return await this.API.provider.getUtxos(addr)
    }

    getNetworkId(): number {
        return 1;
    }

    backend() {
        return this.API;
    }

}
