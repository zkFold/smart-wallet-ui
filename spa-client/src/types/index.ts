export interface WalletBalance {
  [key: string]: string | number;
  lovelace: string;
}

export interface WalletState {
  isInitialized: boolean;
  balance: WalletBalance | null;
  address: string | null;
  network: 'Preview' | 'Preprod' | 'Mainnet';
}

export interface WalletInitializer {
  method: 'Mnemonic' | 'Google';
  data: string;
  rootKey?: string;
}

export interface TransactionRequest {
  recipient: string;
  recipientType: 'Gmail' | 'Bech32';
  asset: string;
  amount: string;
}

export interface BackendConfig {
  url: string;
  apiKey?: string;
}

export interface AppConfig {
  backend: BackendConfig;
  googleClientId: string;
  websiteUrl: string;
}

export interface TransactionResult {
  outcome: 'success' | 'failure' | 'pending';
  data?: any;
  reason?: string;
}

export interface GoogleAuthResult {
  jwt: string;
  error?: string;
}