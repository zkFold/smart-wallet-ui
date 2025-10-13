import { AddressType, WalletInitialiser } from 'zkfold-smart-wallet-api'

// Activated wallets
export interface MultiWalletStorage {
  wallets: { [addr: string]: WalletInitialiser }
}

export interface WalletBalance {
  [asset: string]: any  // Changed from bigint to any to handle BigIntWrap
}

export interface TransactionRequest {
  recipient: string
  recipientType: AddressType
  asset: string
  amount: string
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
export interface AppEvent<E> {
  id: E
  data?: any
}

export interface EventListener<E> {
  (event: AppEvent<E>): void
}

export type WalletEvent =
    'walletInitialized'
  | 'proofComputationComplete'
  | 'transactionComplete'
  | 'transactionFailed'
  | 'walletLoggedOut'
