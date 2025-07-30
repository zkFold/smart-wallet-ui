import * as CSL from '@emurgo/cardano-serialization-lib-browser';
import { generateMnemonic, mnemonicToEntropy, validateMnemonic } from '@scure/bip39';
import { wordlist } from '@scure/bip39/wordlists/english';

export class CryptoService {
  
  /**
   * Initialize the CardanoWasm library
   */
  static async initialize(): Promise<void> {
    // The browser version of CSL is already initialized when imported
    return Promise.resolve();
  }

  /**
   * Generate a new mnemonic phrase
   */
  static generateMnemonic(): string {
    return generateMnemonic(wordlist, 256); // 24 words
  }

  /**
   * Validate a mnemonic phrase
   */
  static validateMnemonic(mnemonic: string): boolean {
    return validateMnemonic(mnemonic, wordlist);
  }

  /**
   * Generate a BIP32 private key for Google OAuth flow
   * This mimics the server-side key generation but happens client-side
   */
  static generateGooglePrivateKey(): CSL.Bip32PrivateKey {
    const rootKey = CSL.Bip32PrivateKey.generate_ed25519_bip32();
    return rootKey
      .derive(this.harden(1852)) // purpose
      .derive(this.harden(1815)) // coin type
      .derive(this.harden(0))    // account #0
      .derive(0)                 // external chain
      .derive(0);                // address index
  }

  /**
   * Derive a wallet from mnemonic
   */
  static walletFromMnemonic(mnemonic: string, network: string): CSL.Bip32PrivateKey {
    if (!this.validateMnemonic(mnemonic)) {
      throw new Error('Invalid mnemonic phrase');
    }

    // Convert mnemonic to entropy
    const entropy = mnemonicToEntropy(mnemonic, wordlist);
    
    // Create root key from entropy
    const rootKey = CSL.Bip32PrivateKey.from_bip39_entropy(
      new Uint8Array(entropy),
      new Uint8Array() // empty passphrase
    );

    // Derive wallet key using Cardano's derivation path
    return rootKey
      .derive(this.harden(1852)) // purpose
      .derive(this.harden(1815)) // coin type
      .derive(this.harden(0))    // account #0
      .derive(0)                 // external chain
      .derive(0);                // address index
  }

  /**
   * Get address from private key
   */
  static getAddressFromPrivateKey(privateKey: CSL.Bip32PrivateKey, network: string): CSL.Address {
    const publicKey = privateKey.to_public();
    const paymentKey = publicKey.to_raw_key();
    
    const paymentKeyHash = paymentKey.hash();
    const paymentCredential = CSL.Credential.from_keyhash(paymentKeyHash);
    
    const networkId = network.toLowerCase() === 'mainnet' ? 
      CSL.NetworkInfo.mainnet().network_id() : 
      CSL.NetworkInfo.testnet_preview().network_id();

    return CSL.BaseAddress.new(
      networkId,
      paymentCredential,
      paymentCredential // Using same key for staking for simplicity
    ).to_address();
  }

  /**
   * Securely clear sensitive data from memory
   */
  static clearSensitiveData(data: string | Uint8Array): void {
    if (typeof data === 'string') {
      // For strings, we can't truly clear them in JS, but we can try
      // This is more of a best practice indication
      data = '';
    } else if (data instanceof Uint8Array) {
      // Clear byte array
      data.fill(0);
    }
  }

  /**
   * Harden a derivation index
   */
  private static harden(num: number): number {
    return 0x80000000 + num;
  }
}

// Initialize crypto service on module load
CryptoService.initialize().catch(console.error);