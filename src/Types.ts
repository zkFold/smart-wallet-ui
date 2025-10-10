import { AddressType, Wallet } from 'zkfold-smart-wallet-api'

export type Network = 'mainnet' | 'preprod' | 'preview'

// Application types

// Activated wallets
export interface MultiWalletStorage {
  wallets: { [addr: string]: Wallet }
}

export interface WalletBalance {
  [asset: string]: any  // Changed from bigint to any to handle BigIntWrap
}

export interface TransactionRequest {
  recipient: string
  recipientType: AddressType
  amount: string
  asset: string
}

export interface TransactionResult {
  txId: string
  recipient: string
  isProofComputing?: boolean
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

// Events
export interface AppEvent {
  type: string
  data?: any
}

export interface EventListener {
  (event: AppEvent): void
}
