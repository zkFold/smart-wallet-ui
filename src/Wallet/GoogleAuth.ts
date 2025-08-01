import { GoogleApi } from 'zkfold-smart-wallet-api'
import { AppConfig } from '../Types'

export class GoogleAuth {
  private googleApi: GoogleApi

  constructor(config: AppConfig) {
    const redirectUrl = `${config.websiteUrl}/oauth2callback`
    this.googleApi = new GoogleApi(config.clientId, config.clientSecret, redirectUrl)
  }

  public getAuthUrl(state: string): string {
    return this.googleApi.getAuthUrl(state)
  }

  public async getJWT(code: string): Promise<string> {
    const result = await this.googleApi.getJWT(code)
    return result || ''  // Handle potential undefined
  }
}