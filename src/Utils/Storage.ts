import { WalletInfo, MultiWalletStorage } from '../Types'

export class StorageManager {
  private readonly MULTI_WALLET_KEY = 'smart-wallets'
  private readonly SESSION_KEY = 'smart-wallet-session'

  // Multi-wallet support with persistent credentials
  public saveWallet(walletInfo: WalletInfo): void {
    try {
      console.log('Saving wallet with ID:', walletInfo.id)
      const multiWallet = this.getMultiWalletStorage()
      console.log('Current storage before save:', multiWallet)
      multiWallet.wallets[walletInfo.id] = walletInfo
      console.log('Storage after adding wallet:', multiWallet)
      localStorage.setItem(this.MULTI_WALLET_KEY, JSON.stringify(multiWallet))
      
      // Verify the save worked
      const verification = localStorage.getItem(this.MULTI_WALLET_KEY)
      console.log('Verification read from localStorage:', verification ? JSON.parse(verification) : null)
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

  public removeWallet(walletId: string): void {
    try {
      const multiWallet = this.getMultiWalletStorage()
      delete multiWallet.wallets[walletId]
      if (multiWallet.activeWalletId === walletId) {
        multiWallet.activeWalletId = undefined
      }
      localStorage.setItem(this.MULTI_WALLET_KEY, JSON.stringify(multiWallet))
    } catch (error) {
      console.warn('Failed to remove wallet from localStorage:', error)
    }
  }

  public setActiveWallet(walletId: string): void {
    try {
      console.log('Setting active wallet to:', walletId)
      const multiWallet = this.getMultiWalletStorage()
      console.log('Available wallets:', Object.keys(multiWallet.wallets))
      if (multiWallet.wallets[walletId]) {
        multiWallet.activeWalletId = walletId
        localStorage.setItem(this.MULTI_WALLET_KEY, JSON.stringify(multiWallet))
        console.log('Successfully set active wallet to:', walletId)
      } else {
        console.warn('Wallet not found when setting active:', walletId)
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

  private getMultiWalletStorage(): MultiWalletStorage {
    try {
      const stored = localStorage.getItem(this.MULTI_WALLET_KEY)
      if (stored) {
        return JSON.parse(stored)
      } else {
        // Initialize empty storage if it doesn't exist
        const defaultStorage: MultiWalletStorage = { wallets: {} }
        localStorage.setItem(this.MULTI_WALLET_KEY, JSON.stringify(defaultStorage))
        return defaultStorage
      }
    } catch (error) {
      console.warn('Failed to parse multi-wallet storage:', error)
      const defaultStorage: MultiWalletStorage = { wallets: {} }
      localStorage.setItem(this.MULTI_WALLET_KEY, JSON.stringify(defaultStorage))
      return defaultStorage
    }
  }
}