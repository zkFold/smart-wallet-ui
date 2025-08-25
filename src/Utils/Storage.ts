import { WalletInfo, MultiWalletStorage } from '../Types'
import { serialize, deserialize } from 'zkfold-smart-wallet-api'

export class StorageManager {
  private readonly MULTI_WALLET_KEY = 'smart-wallets'
  private readonly SESSION_KEY = 'smart-wallet-session'

  // Multi-wallet support with persistent credentials
  public saveWallet(walletInfo: WalletInfo): void {
    try {
      const multiWallet = this.getMultiWalletStorage()
      multiWallet.wallets[walletInfo.id] = walletInfo
      localStorage.setItem(this.MULTI_WALLET_KEY, serialize(multiWallet))
    } catch (error) {
      console.warn('Failed to save wallet to localStorage:', error)
    }
  }

  public getWallet(walletId: string): WalletInfo | null {
    try {
      const multiWallet = this.getMultiWalletStorage()
      return multiWallet.wallets[walletId] || null
    } catch (error) {
      console.warn('Failed to retrieve wallet from localStorage:', error)
      return null
    }
  }

  public getAllWallets(): WalletInfo[] {
    try {
      const multiWallet = this.getMultiWalletStorage()
      return Object.values(multiWallet.wallets)
    } catch (error) {
      console.warn('Failed to retrieve wallets from localStorage:', error)
      return []
    }
  }

  public findWalletByEmail(email: string): WalletInfo | null {
    try {
      const allWallets = this.getAllWallets()
      return allWallets.find(wallet => wallet.state.userEmail === email) || null
    } catch (error) {
      console.warn('Failed to find wallet by email:', error)
      return null
    }
  }

  public removeWallet(walletId: string): void {
    try {
      const multiWallet = this.getMultiWalletStorage()
      delete multiWallet.wallets[walletId]
      if (multiWallet.activeWalletId === walletId) {
        multiWallet.activeWalletId = undefined
      }
      localStorage.setItem(this.MULTI_WALLET_KEY, serialize(multiWallet))
    } catch (error) {
      console.warn('Failed to remove wallet from localStorage:', error)
    }
  }

  public setActiveWallet(walletId: string): void {
    try {
      const multiWallet = this.getMultiWalletStorage()
      if (multiWallet.wallets[walletId]) {
        multiWallet.activeWalletId = walletId
        localStorage.setItem(this.MULTI_WALLET_KEY, serialize(multiWallet))
      }
    } catch (error) {
      console.warn('Failed to set active wallet:', error)
    }
  }

  public getActiveWallet(): WalletInfo | null {
    try {
      const multiWallet = this.getMultiWalletStorage()
      return multiWallet.activeWalletId ? multiWallet.wallets[multiWallet.activeWalletId] || null : null
    } catch (error) {
      console.warn('Failed to get active wallet:', error)
      return null
    }
  }

  public clearActiveWallet(): void {
    try {
      const multiWallet = this.getMultiWalletStorage()
      multiWallet.activeWalletId = undefined
      localStorage.setItem(this.MULTI_WALLET_KEY, serialize(multiWallet))
    } catch (error) {
      console.warn('Failed to clear active wallet:', error)
    }
  }

  public clearWalletState(): void {
    try {
      localStorage.removeItem(this.MULTI_WALLET_KEY)
    } catch (error) {
      console.warn('Failed to clear wallet state from localStorage:', error)
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
