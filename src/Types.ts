import type { BalanceResponse, Transaction } from 'zkfold-smart-wallet-api'

export interface AppConfig {
  backendUrl: string
  backendApiKey?: string
  rollupUrl: string
  rollupApiKey?: string
}

export type AppView = 'init' | 'wallet' | 'error'

export type DemoActionStatus = 'pending' | 'success' | 'error'

export interface DemoAction {
  id: string
  label: string
  status: DemoActionStatus
  detail: string
  timestamp: string
}

export interface WalletViewModel {
  l1Address: string
  l2Address: string
  l1Balance: BalanceResponse
  l2Balance: BalanceResponse
  l1Transactions: Transaction[]
  l2Transactions: Transaction[]
  actions: DemoAction[]
}
