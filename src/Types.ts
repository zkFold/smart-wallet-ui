// Environment configuration
export interface AppConfig {
  websiteUrl: string
  backendUrl: string
  backendApiKey?: string
  proverUrl: string
}

export type AppView = 'init' | 'wallet' | 'success' | 'failed'

export interface WalletBalance {
  [asset: string]: any  // Changed from bigint to any to handle BigIntWrap
}
