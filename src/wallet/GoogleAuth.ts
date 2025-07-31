import { GoogleApi } from 'zkfold-smart-wallet-api'
import { AppConfig } from '../types'

export class GoogleAuth {
  private googleApi: GoogleApi

  constructor(config: AppConfig) {
    const redirectUrl = `${config.websiteUrl}/oauth2callback`
    this.googleApi = new GoogleApi(config.clientId, redirectUrl)
  }

  public async getAuthUrl(state: string): Promise<string> {
    return await this.googleApi.getAuthUrl(state)
  }

  public async getJWT(authorizationCode: string): Promise<string> {
    const result = await this.googleApi.getJWT(authorizationCode)
    return result || ''  // Handle potential undefined
  }
}