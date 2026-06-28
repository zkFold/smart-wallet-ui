import type { BalanceResponse, Transaction, WalletSubmitKind } from 'zkfold-smart-wallet-api'

export interface AppConfig {
  backendUrl: string
  backendApiKey: string | null
  rollupUrl: string
  rollupApiKey: string | null
}

export type AppView = 'init' | 'wallet' | 'error'

export type DemoLayer = 'l1' | 'l2'

export type DemoActionStatus =
  | 'submitting'
  | 'submitted'
  | 'queued'
  | 'batched'
  | 'confirmed'
  | 'failed'

export interface BalanceEffect {
  layer: DemoLayer
  asset: string
  quantity: number
  baseline: number
}

export interface DemoAction {
  id: string
  label: string
  status: DemoActionStatus
  detail: string
  timestamp: string
  kind?: WalletSubmitKind
  layer?: DemoLayer
  txId?: string
  txHash?: string
  effects: BalanceEffect[]
}

export interface WalletViewModel {
  l1Address: string
  l2Address: string
  l1Balance: BalanceResponse
  l2Balance: BalanceResponse
  l1SpendableBalance: BalanceResponse
  l2SpendableBalance: BalanceResponse
  l1Transactions: Transaction[]
  l2Transactions: Transaction[]
  actions: DemoAction[]
}
