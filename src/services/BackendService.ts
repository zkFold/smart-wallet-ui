import { Backend } from 'zkfold-smart-wallet-api'
import * as CSL from '@emurgo/cardano-serialization-lib-browser'
import { AppConfig } from '../types'

export class BackendService {
  private backend: Backend

  constructor(config: AppConfig) {
    this.backend = config.backendApiKey
      ? new Backend(config.backendUrl, config.backendApiKey)
      : new Backend(config.backendUrl)
  }

  public async checkTransactionStatus(txId: string, recipient: string): Promise<any> {
    try {
      const address = CSL.Address.from_bech32(recipient)
      const utxos = await this.backend.addressUtxo(address)
      
      for (const utxo of utxos) {
        if ((utxo as any).ref.transaction_id === txId) {
          return { outcome: "success", data: utxo }
        }
      }
      
      return { outcome: "pending" }
    } catch (error) {
      console.error('Failed to check transaction status:', error)
      return { outcome: "failure", reason: error }
    }
  }
}