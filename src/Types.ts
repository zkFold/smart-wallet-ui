// Application types
export interface WalletConfig {
  method: WalletMethod
  network: Network
  data?: string
}

export interface WalletState {
  isInitialized: boolean
  address?: string
  balance?: WalletBalance
  network?: Network
  method?: WalletMethod
}

export interface WalletBalance {
  [asset: string]: any  // Changed from bigint to any to handle BigIntWrap
}

export interface TransactionRequest {
  recipient: string
  recipientType: RecipientType
  amount: string
  asset: string
}

export interface TransactionResult {
  txId: string
  recipient: string
}

export type WalletMethod = 'Mnemonic' | 'Google Oauth'
export type Network = 'Preview' | 'Preprod' | 'Mainnet'
export type RecipientType = 'Bech32' | 'Gmail'
export type AppView = 'init' | 'wallet' | 'success' | 'failed'

// Environment configuration
export interface AppConfig {
  clientId: string
  clientSecret: string
  websiteUrl: string
  backendUrl: string
  backendApiKey?: string
}

// Events
export interface AppEvent {
  type: string
  data?: any
}

export interface EventListener {
  (event: AppEvent): void
}