import { WalletState } from '../types'

export class StorageManager {
  private readonly WALLET_STATE_KEY = 'smart-wallet-state'
  private readonly SESSION_KEY = 'smart-wallet-session'

  public saveWalletState(state: WalletState): void {
    try {
      localStorage.setItem(this.WALLET_STATE_KEY, JSON.stringify(state))
    } catch (error) {
      console.warn('Failed to save wallet state to localStorage:', error)
    }
  }

  public getWalletState(): WalletState | null {
    try {
      const stored = localStorage.getItem(this.WALLET_STATE_KEY)
      return stored ? JSON.parse(stored) : null
    } catch (error) {
      console.warn('Failed to retrieve wallet state from localStorage:', error)
      return null
    }
  }

  public clearWalletState(): void {
    try {
      localStorage.removeItem(this.WALLET_STATE_KEY)
      localStorage.removeItem(this.SESSION_KEY)
    } catch (error) {
      console.warn('Failed to clear wallet state from localStorage:', error)
    }
  }

  public saveSessionData(key: string, data: any): void {
    try {
      const sessionData = this.getSessionData()
      sessionData[key] = data
      localStorage.setItem(this.SESSION_KEY, JSON.stringify(sessionData))
    } catch (error) {
      console.warn('Failed to save session data:', error)
    }
  }

  public getSessionData(): any {
    try {
      const stored = localStorage.getItem(this.SESSION_KEY)
      return stored ? JSON.parse(stored) : {}
    } catch (error) {
      console.warn('Failed to retrieve session data:', error)
      return {}
    }
  }

  public getSessionItem(key: string): any {
    const sessionData = this.getSessionData()
    return sessionData[key]
  }

  public removeSessionItem(key: string): void {
    try {
      const sessionData = this.getSessionData()
      delete sessionData[key]
      localStorage.setItem(this.SESSION_KEY, JSON.stringify(sessionData))
    } catch (error) {
      console.warn('Failed to remove session item:', error)
    }
  }
}