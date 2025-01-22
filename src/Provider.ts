import { UTxO } from './Types'

export class Provider {

    async getUtxos(addr: string) : Promise<UTxO[]>;

    async submitTx(tx: string): Promise<string>;

    getNetworkId(): number;

}
