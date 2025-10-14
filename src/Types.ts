import { WalletInitialiser } from 'zkfold-smart-wallet-api'

// Activated wallets
export interface MultiWalletStorage {
  wallets: { [addr: string]: WalletInitialiser }
}

export interface WalletBalance {
  [asset: string]: any  // Changed from bigint to any to handle BigIntWrap
}

export type WalletMethod = 'Google Oauth'
export type AppView = 'init' | 'wallet' | 'success' | 'failed'

// Environment configuration
export interface AppConfig {
  clientId: string
  clientSecret: string
  websiteUrl: string
  backendUrl: string
  backendApiKey?: string
  proverUrl: string
}
