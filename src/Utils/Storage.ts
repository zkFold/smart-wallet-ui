import { MultiWalletStorage } from '../Types'
import { serialize, deserialize, WalletInitialiser } from 'zkfold-smart-wallet-api'

export class Storage {
  private readonly MULTI_WALLET_KEY = 'smart-wallets'
  private readonly SESSION_KEY = 'smart-wallet-session'

  // Multi-wallet support with persistent credentials
  public saveWallet(addr: string, wallet: WalletInitialiser): void {
    try {
      const multiWallet = this.getMultiWalletStorage()
      multiWallet.wallets[addr] = wallet
      localStorage.setItem(this.MULTI_WALLET_KEY, serialize(multiWallet))
    } catch (error) {
      console.warn('Failed to save wallet to localStorage:', error)
    }
  }

  public getWallet(addr: string): WalletInitialiser | null {
    try {
      const multiWallet = this.getMultiWalletStorage()
      return multiWallet.wallets[addr] || null
    } catch (error) {
      console.warn('Failed to retrieve wallet from localStorage:', error)
      return null
    }
  }

  public getAllWallets(): WalletInitialiser[] {
    try {
      const multiWallet = this.getMultiWalletStorage()
      return Object.values(multiWallet.wallets)
    } catch (error) {
      console.warn('Failed to retrieve wallets from localStorage:', error)
      return []
    }
  }

  public removeWallet(addr: string): void {
    try {
      const multiWallet = this.getMultiWalletStorage()
      delete multiWallet.wallets[addr]
      localStorage.setItem(this.MULTI_WALLET_KEY, serialize(multiWallet))
    } catch (error) {
      console.warn('Failed to remove wallet from localStorage:', error)
    }
  }

  public removeAllWallets(): void {
    try {
      localStorage.removeItem(this.MULTI_WALLET_KEY)
    } catch (error) {
      console.warn('Failed to remove all wallets from localStorage:', error)
    }
  }

  public saveSessionData(key: string, data: any): void {
    try {
      const sessionData = this.getSessionData()
      sessionData[key] = data
      sessionStorage.setItem(this.SESSION_KEY, serialize(sessionData))
    } catch (error) {
      console.warn('Failed to save session data:', error)
    }
  }

  public getSessionData(): any {
    try {
      const stored = sessionStorage.getItem(this.SESSION_KEY)
      return stored ? deserialize(stored) : {}
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
      sessionStorage.setItem(this.SESSION_KEY, serialize(sessionData))
    } catch (error) {
      console.warn('Failed to remove session item:', error)
    }
  }

  private getMultiWalletStorage(): MultiWalletStorage {
    try {
      const stored = localStorage.getItem(this.MULTI_WALLET_KEY)
      if (stored) {
        return deserialize(stored)
      } else {
        // Initialize empty storage if it doesn't exist
        const defaultStorage: MultiWalletStorage = { wallets: {} }
        localStorage.setItem(this.MULTI_WALLET_KEY, serialize(defaultStorage))
        return defaultStorage
      }
    } catch (error) {
      console.warn('Failed to parse multi-wallet storage:', error)
      const defaultStorage: MultiWalletStorage = { wallets: {} }
      localStorage.setItem(this.MULTI_WALLET_KEY, serialize(defaultStorage))
      return defaultStorage
    }
  }
}
