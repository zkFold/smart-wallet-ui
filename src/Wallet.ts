import CardanoWasm from '@emurgo/cardano-serialization-lib-nodejs';
import * as bip39 from '@scure/bip39';
import { wordlist } from '@scure/bip39/wordlists/english';

function harden(num: number): number {
  return 0x80000000 + num;
}

export class Wallet {
    private rootKey: string;
    private accountKey: string;
    private utxoPubKey: string;
    private stakeKey: string;

    constructor(name: string, mnemonic: string, password: string = '') {
        this.name = name;
        const entropy = bip39.mnemonicToEntropy(mnemonic, wordlist);
        this.rootKey = CardanoWasm.Bip32PrivateKey.from_bip39_entropy(
              Buffer.from(entropy, 'hex'),
              Buffer.from(password),
            );
        this.deriveKeys();
    }

    getName(): string {
        return this.name;
    }

    getRootKey(): string {
        return this.rootKey;
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
    getAddresses(): Address[] {
        const baseAddr = CardanoWasm.BaseAddress.new(
          CardanoWasm.NetworkInfo.mainnet().network_id(),
          CardanoWasm.Credential.from_keyhash(this.utxoPubKey.to_raw_key().hash()),
          CardanoWasm.Credential.from_keyhash(this.stakeKey.to_raw_key().hash()),
        );
        
        // enterprise address without staking ability, for use by exchanges/etc
        const enterpriseAddr = CardanoWasm.EnterpriseAddress.new(
          CardanoWasm.NetworkInfo.mainnet().network_id(),
          CardanoWasm.Credential.from_keyhash(this.utxoPubKey.to_raw_key().hash())
        );
        
        // pointer address - similar to Base address but can be shorter, see formal spec for explanation
        const ptrAddr = CardanoWasm.PointerAddress.new(
          CardanoWasm.NetworkInfo.mainnet().network_id(),
          CardanoWasm.Credential.from_keyhash(this.utxoPubKey.to_raw_key().hash()),
          CardanoWasm.Pointer.new(
            100, // slot
            2,   // tx index in slot
            0    // cert indiex in tx
          )
        );
        
        // reward address - used for withdrawing accumulated staking rewards
        const rewardAddr = CardanoWasm.RewardAddress.new(
          CardanoWasm.NetworkInfo.mainnet().network_id(),
          CardanoWasm.Credential.from_keyhash(this.stakeKey.to_raw_key().hash())
        );        
        return [baseAddr.to_address(), enterpriseAddr.to_address(), ptrAddr.to_address(), rewardAddr.to_address()]
    }

    getChangeAddress(): Address {
        return this.getAddresses()[0];
    }

    getRewardAddress(): Address {
        return this.getAddresses()[3];
    }

}
