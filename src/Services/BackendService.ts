import { Backend, ClientCredentials } from 'zkfold-smart-wallet-api'
import { AppConfig } from '../Types'

export class BackendService {
  private backend: Backend

  constructor(config: AppConfig) {
    this.backend = config.backendApiKey
      ? new Backend(config.backendUrl, config.backendApiKey)
      : new Backend(config.backendUrl)
  }

  public async credentials(): Promise<ClientCredentials | null> {
    try {
      const cc = await this.backend.credentials();
      return cc
    } catch (error) {
      console.error('Failed to check transaction status:', error)
      return null
    }
  }
}
