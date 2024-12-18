import { UTxO } from './Types'

export class Provider {

    async getUtxos(addr: string) : Promise<UTxO[]>;

    getNetworkId(): number;

    backend();

}
