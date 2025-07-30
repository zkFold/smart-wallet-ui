import { CryptoService } from './cryptoService';
import { backendApi } from './backendApi';
import { WalletInitializer, WalletBalance, TransactionRequest } from '../types';

export class WalletService {
  private initializer: WalletInitializer | null = null;
  private network: string = 'preprod';
  private address: string | null = null;

  /**
   * Initialize wallet from mnemonic
   */
  async initializeFromMnemonic(mnemonic: string, network: string): Promise<void> {
    if (!CryptoService.validateMnemonic(mnemonic)) {
      throw new Error('Invalid mnemonic phrase');
    }

    this.network = network.toLowerCase();
    this.initializer = {
      method: 'Mnemonic',
      data: mnemonic
    };

    // Generate address client-side
    const privateKey = CryptoService.walletFromMnemonic(mnemonic, network);
    const address = CryptoService.getAddressFromPrivateKey(privateKey, network);
    this.address = address.to_bech32();

    // Send initialization data to backend (without private keys)
    await this.initializeBackend();
  }

  /**
   * Initialize wallet from Google OAuth
   */
  async initializeFromGoogle(jwt: string, network: string): Promise<void> {
    this.network = network.toLowerCase();
    
    // Generate private key client-side
    const privateKey = CryptoService.generateGooglePrivateKey();
    const address = CryptoService.getAddressFromPrivateKey(privateKey, network);
    this.address = address.to_bech32();
    
    this.initializer = {
      method: 'Google',
      data: jwt,
      rootKey: privateKey.to_hex()
    };

    // Send initialization data to backend (without private keys)
    await this.initializeBackend();
    
    // Clear the private key from memory after use
    CryptoService.clearSensitiveData(privateKey.to_hex());
  }

  /**
   * Initialize backend session
   */
  private async initializeBackend(): Promise<void> {
    if (!this.initializer) {
      throw new Error('Wallet not properly initialized');
    }

    // Send initialization request to backend
    const response = await backendApi.initializeWallet({
      method: this.initializer.method,
      network: this.network,
      address: this.address!,
      // Send JWT for Google auth, but not mnemonic or private keys
      ...(this.initializer.method === 'Google' && { jwt: this.initializer.data })
    });

    if (!response.success) {
      throw new Error('Failed to initialize backend wallet session');
    }
  }

  /**
   * Get wallet balance from backend
   */
  async getBalance(): Promise<WalletBalance> {
    if (!this.address) {
      throw new Error('Wallet not initialized');
    }

    return backendApi.getBalance(this.address);
  }

  /**
   * Get wallet address
   */
  async getAddress(): Promise<string> {
    if (!this.address) {
      throw new Error('Wallet not initialized');
    }

    return this.address;
  }

  /**
   * Get address for Gmail recipient
   */
  async getAddressForGmail(email: string): Promise<string> {
    return backendApi.getGmailAddress(email);
  }

  /**
   * Send transaction
   */
  async sendTransaction(request: TransactionRequest): Promise<string> {
    if (!this.address) {
      throw new Error('Wallet not initialized');
    }

    // For now, delegate to backend
    // In a full implementation, we would build and sign the transaction client-side
    return backendApi.sendTransaction({
      fromAddress: this.address,
      toAddress: request.recipient,
      recipientType: request.recipientType,
      asset: request.asset,
      amount: request.amount
    });
  }

  /**
   * Check if wallet is initialized
   */
  isInitialized(): boolean {
    return this.address !== null;
  }

  /**
   * Get current network
   */
  getNetwork(): string {
    return this.network;
  }

  /**
   * Clear wallet data from memory
   */
  clear(): void {
    this.address = null;
    
    // Clear sensitive initializer data
    if (this.initializer) {
      if (this.initializer.method === 'Mnemonic') {
        CryptoService.clearSensitiveData(this.initializer.data);
      }
      if (this.initializer.rootKey) {
        CryptoService.clearSensitiveData(this.initializer.rootKey);
      }
      this.initializer = null;
    }
  }

  /**
   * Generate a new mnemonic phrase
   */
  static generateMnemonic(): string {
    return CryptoService.generateMnemonic();
  }

  /**
   * Validate mnemonic phrase
   */
  static validateMnemonic(mnemonic: string): boolean {
    return CryptoService.validateMnemonic(mnemonic);
  }
}

export const walletService = new WalletService();