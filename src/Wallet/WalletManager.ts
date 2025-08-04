import { Wallet, WalletType, SmartTxRecipient, Backend, BigIntWrap, Network, Prover } from 'zkfold-smart-wallet-api'
import * as CSL from '@emurgo/cardano-serialization-lib-browser'
import { AppConfig, WalletState, WalletInfo, TransactionRequest, TransactionResult } from '../Types'
import { StorageManager } from '../Utils/Storage'
import { EventEmitter } from '../Utils/EventEmitter'
import { GoogleAuth } from './GoogleAuth'
import { harden, decodeJWT } from '../Utils/Helpers'

export class WalletManager extends EventEmitter {
  private config: AppConfig
  private storage: StorageManager
  private googleAuth: GoogleAuth
  private wallet: Wallet | null = null
  private backend: Backend | null = null
  private currentWalletId: string | null = null

  constructor(config: AppConfig, storage: StorageManager) {
    super()
    this.config = config
    this.storage = storage
    this.googleAuth = new GoogleAuth(config)
  }

  public async initializeWallet(network: Network): Promise<void> {
    try {
      // Initialize backend
      this.backend = this.config.backendApiKey
        ? new Backend(this.config.backendUrl, this.config.backendApiKey)
        : new Backend(this.config.backendUrl)

      this.prover = new Prover(this.config.proverUrl);

      // Generate state for OAuth flow
      const state = this.generateOAuthState()
      this.storage.saveSessionData('oauth_state', state)
      this.storage.saveSessionData('network', network)

      // Redirect to Google OAuth
      const authUrl = this.googleAuth.getAuthUrl(state)
      window.location.href = authUrl
    } catch (error) {
      console.error('Failed to initialize wallet:', error)
      this.emit('walletInitializationFailed', error)
      throw error
    }
  }

  public async handleOAuthCallback(callbackData: string): Promise<void> {
    try {
      // Initialize backend (needed for OAuth callback flow)
      this.backend = this.config.backendApiKey
        ? new Backend(this.config.backendUrl, this.config.backendApiKey)
        : new Backend(this.config.backendUrl)

      // Parse URL parameters to get authorization code
      const params = new URLSearchParams(callbackData)
      const code = params.get('code')
      const state = params.get('state')
      const error = params.get('error')

      if (error) {
        throw new Error(`OAuth error: ${error}`)
      }

      const savedState = this.storage.getSessionItem('oauth_state')
      if (state !== savedState) {
        throw new Error('State mismatch. Possible CSRF attack')
      }

      if (!code) {
        throw new Error('Missing authorization code')
      }

      // Get JWT token using authorization code (now async)
      const jwt = await this.googleAuth.getJWT(code)

      if (!jwt) {
        throw new Error('Failed to get JWT from authorization code')
      }

      // Generate root key for Google OAuth wallet
      const prvKey = CSL.Bip32PrivateKey
        .generate_ed25519_bip32()
        .derive(harden(1852)) // purpose
        .derive(harden(1815)) // coin type
        .derive(harden(0)) // account #0
        .derive(0)
        .derive(0)

      const initialiser = {
        method: WalletType.Google,
        data: jwt,
        rootKey: prvKey.to_hex()
      }

      const network = this.storage.getSessionItem('network')
      await this.completeWalletInitialization(initialiser, network)

      this.storage.removeSessionItem('oauth_state')
      this.storage.removeSessionItem('network')

      // Redirect to root address after successful OAuth callback
      window.history.replaceState({}, '', '/')

    } catch (error) {
      console.error('OAuth callback failed:', error)
      this.emit('walletInitializationFailed', error)
      // Redirect to home page on error
      window.history.replaceState({}, '', '/')
      throw error
    }
  }

  private async completeWalletInitialization(initialiser: any, network: Network): Promise<void> {
    if (!this.backend) {
      throw new Error('Backend not initialized')
    }

    // Create wallet instance
    this.wallet = new Wallet(this.backend, this.prover, initialiser, '', network)

    // Get wallet balance and address
    const balance = await this.wallet.getBalance()
    const address = await this.wallet.getAddress().then((x: any) => x.to_bech32())

    console.log(`Initialized a ${network} wallet with address ${address}`)
    console.log('Balance:', balance)

    // Extract email from JWT token
    let userEmail: string | undefined
    if (initialiser.data) {
      const jwtPayload = decodeJWT(initialiser.data)
      if (jwtPayload && jwtPayload.email) {
        userEmail = jwtPayload.email
      }
    }

    // Check if a wallet with the same email already exists
    let existingWallet: WalletInfo | null = null
    if (userEmail) {
      existingWallet = this.storage.findWalletByEmail(userEmail)
    }

    if (existingWallet) {
      // Reuse existing wallet credential to avoid expensive proof recomputation
      console.log(`Found existing wallet for email ${userEmail}. Reusing existing credential.`)
      
      // Update wallet balance and address in case they changed
      existingWallet.state.balance = balance
      existingWallet.state.address = address
      existingWallet.state.isInitialized = true
      
      // Save the updated wallet info (without replacing the credential)
      this.storage.saveWallet(existingWallet)
      this.storage.setActiveWallet(existingWallet.id)
      this.currentWalletId = existingWallet.id
      
      // Recreate wallet instance with existing credential
      this.wallet = new Wallet(this.backend!, this.prover, existingWallet.credential, '', existingWallet.network!)
      
      // Emit wallet initialized event with updated state
      this.emit('walletInitialized', existingWallet.state)
    } else {
      // Create new wallet as no existing wallet found for this email
      console.log(`No existing wallet found for email ${userEmail}. Creating new wallet.`)
      
      // Create wallet state
      const walletState: WalletState = {
        isInitialized: true,
        address,
        balance,
        network: network as any,
        method: 'Google Oauth',
        userEmail
      }

      // Generate unique wallet ID based on address
      const walletId = this.generateWalletId(address)

      // Create wallet info with persistent credential
      const walletInfo: WalletInfo = {
        id: walletId,
        state: walletState,
        network: network,
        credential: initialiser
      }

      // Save wallet to multi-wallet storage
      this.storage.saveWallet(walletInfo)
      this.storage.setActiveWallet(walletId)
      this.currentWalletId = walletId
      
      // Emit wallet initialized event
      this.emit('walletInitialized', walletState)
    }
  }

  public async restoreWallet(walletState: WalletState): Promise<void> {
    if (!walletState.isInitialized || !walletState.address) {
      throw new Error('Invalid wallet state')
    }

    // Initialize backend
    this.backend = this.config.backendApiKey
      ? new Backend(this.config.backendUrl, this.config.backendApiKey)
      : new Backend(this.config.backendUrl)

    // Restore from multi-wallet storage
    const activeWallet = this.storage.getActiveWallet()
    if (activeWallet && activeWallet.credential && activeWallet.network) {
      try {
        // Recreate wallet instance using stored credential
        this.wallet = new Wallet(this.backend, this.prover, activeWallet.credential, '', activeWallet.network)
        this.currentWalletId = activeWallet.id
        console.log('Wallet instance restored from persistent storage')
        return
      } catch (error) {
        console.warn('Failed to restore wallet instance from persistent storage:', error)
      }
    }

    // If we can't restore the wallet instance, the user will need to re-initialize
    console.log('Wallet state loaded but instance could not be restored. Re-initialization required.')
  }

  public async switchToWallet(walletId: string): Promise<void> {
    const walletInfo = this.storage.getWallet(walletId)
    if (!walletInfo) {
      throw new Error(`Wallet with ID ${walletId} not found`)
    }

    if (!walletInfo.credential || !walletInfo.network) {
      throw new Error('Wallet credential not available')
    }

    try {
      // Initialize backend if needed
      if (!this.backend) {
        this.backend = this.config.backendApiKey
          ? new Backend(this.config.backendUrl, this.config.backendApiKey)
          : new Backend(this.config.backendUrl)
      }

      // Create wallet instance
      this.wallet = new Wallet(this.backend, this.prover, walletInfo.credential, '', walletInfo.network)
      this.currentWalletId = walletId
      this.storage.setActiveWallet(walletId)

      console.log(`Switched to wallet: ${walletInfo.id}`)
      this.emit('walletSwitched', walletInfo.state)
    } catch (error) {
      console.error('Failed to switch wallet:', error)
      throw error
    }
  }

  public getAllWallets(): WalletInfo[] {
    return this.storage.getAllWallets()
  }

  public getCurrentWalletId(): string | null {
    return this.currentWalletId
  }

  public removeWallet(walletId: string): void {
    this.storage.removeWallet(walletId)
    if (this.currentWalletId === walletId) {
      this.wallet = null
      this.currentWalletId = null
      // Switch to another wallet if available
      const availableWallets = this.getAllWallets()
      if (availableWallets.length > 0) {
        this.switchToWallet(availableWallets[0].id)
      }
    }
  }

  public async sendTransaction(request: TransactionRequest): Promise<void> {
    try {
      if (!this.wallet) {
        console.log('Wallet instance not available, attempting to restore...')
        // Try to restore wallet if it's not available but we have active wallet
        const activeWallet = this.storage.getActiveWallet()
        if (activeWallet) {
          await this.restoreWallet(activeWallet.state)
        }

        // If wallet is still not available, throw error
        if (!this.wallet) {
          throw new Error('Wallet not initialized')
        }
        console.log('Wallet instance restored successfully')
      } else {
        console.log('Using existing wallet instance (no restoration needed)')
      }

      console.log(`Sending ${request.amount} ${request.asset} to ${request.recipient} using ${request.recipientType}`)

      // Create asset dictionary
      const assetDict: { [key: string]: BigIntWrap } = {}
      assetDict[request.asset] = new BigIntWrap(request.amount)

      // Create recipient based on type
      let recipient: SmartTxRecipient
      switch (request.recipientType) {
        case 'Bech32':
          recipient = new SmartTxRecipient(WalletType.Google, request.recipient, assetDict)
          break
        case 'Gmail':
          recipient = new SmartTxRecipient(WalletType.Google, request.recipient, assetDict)
          break
        default:
          throw new Error(`Unsupported recipient type: ${request.recipientType}`)
      }

      // Get recipient address for tracking
      let recipientAddress: string
      if (request.recipientType === 'Gmail') {
        recipientAddress = await this.wallet.addressForGmail(request.recipient).then((x: any) => x.to_bech32())
      } else {
        recipientAddress = request.recipient
      }

      // Navigate to success view immediately with proof computing state
      const initialResult: TransactionResult = {
        txId: 'Computing...', // Temporary value while computing
        recipient: recipientAddress,
        isProofComputing: true
      }
      this.emit('transactionComplete', initialResult)

      // Send transaction (this includes the proof computation)
      const txId = await this.wallet.sendTo(recipient)
      console.log(`Transaction ID: ${txId}`)

      // Emit proof computation complete event
      const finalResult: TransactionResult = {
        txId,
        recipient: recipientAddress,
        isProofComputing: false
      }
      this.emit('proofComputationComplete', finalResult)

    } catch (error) {
      console.error('Transaction failed:', error)
      this.emit('transactionFailed', error)
      throw error
    }
  }

  public async checkTransactionStatus(txId: string, recipient: string): Promise<any> {
    try {
      if (!this.backend) {
        throw new Error('Backend not initialized')
      }

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

  public async getWalletBalance(): Promise<any> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized')
    }
    return await this.wallet.getBalance()
  }

  public async getWalletAddress(): Promise<string> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized')
    }
    return await this.wallet.getAddress().then((x: any) => x.to_bech32())
  }

  public async refreshWalletState(): Promise<WalletState> {
    // Ensure backend is available
    if (!this.backend) {
      this.backend = this.config.backendApiKey
        ? new Backend(this.config.backendUrl, this.config.backendApiKey)
        : new Backend(this.config.backendUrl)
      console.log('Backend instance initialized for refresh')
    }

    if (!this.wallet) {
      console.log('Wallet instance not available during refresh, attempting to restore...')
      // Try to restore wallet if it's not available
      const activeWallet = this.storage.getActiveWallet()
      if (activeWallet) {
        await this.restoreWallet(activeWallet.state)
      }
      
      if (!this.wallet) {
        throw new Error('Wallet not initialized and could not be restored')
      }
      console.log('Wallet instance restored for refresh')
    } else {
      console.log('Using existing wallet instance for refresh')
    }

    // Get fresh balance and address
    const balance = await this.wallet.getBalance()
    const address = await this.wallet.getAddress().then((x: any) => x.to_bech32())

    // Get current wallet info to preserve other data
    const activeWallet = this.storage.getActiveWallet()
    if (!activeWallet) {
      throw new Error('No active wallet found')
    }

    // Update wallet state with fresh data
    const updatedState: WalletState = {
      ...activeWallet.state,
      balance,
      address
    }

    // Update stored wallet info
    const updatedWalletInfo: WalletInfo = {
      ...activeWallet,
      state: updatedState
    }
    
    this.storage.saveWallet(updatedWalletInfo)

    console.log('Wallet state refreshed with updated balance:', balance)
    return updatedState
  }

  public isWalletReady(): boolean {
    return this.wallet !== null
  }

  public clearWallet(): void {
    this.wallet = null
    this.backend = null
    this.currentWalletId = null
    this.storage.clearWalletState()
  }

  public logout(): void {
    // Clear the current session without deleting wallet data from localStorage
    this.wallet = null
    this.backend = null
    this.currentWalletId = null
    // Clear any session data
    sessionStorage.clear()
    // Emit logout event
    this.emit('walletLoggedOut', {})
  }

  private generateOAuthState(): string {
    const array = new Uint8Array(32)
    crypto.getRandomValues(array)
    return Array.from(array, byte => byte.toString(16).padStart(2, '0')).join('')
  }

  private generateWalletId(address: string): string {
    // Use address as wallet ID since it already contains network information
    return address.slice(0, 16) // First 16 characters of the address
  }
}
