import { Wallet, SmartTxRecipient, Backend, BigIntWrap, Prover, AddressType } from 'zkfold-smart-wallet-api'
import * as CSL from '@emurgo/cardano-serialization-lib-browser'
import { AppConfig, TransactionRequest, TransactionResult } from '../Types'
import { StorageManager } from '../Utils/Storage'
import { EventEmitter } from '../Utils/EventEmitter'
import { GoogleAuth } from './GoogleAuth'

export class WalletManager extends EventEmitter {
  private config: AppConfig
  private storage: StorageManager
  private googleAuth: GoogleAuth
  private backend: Backend
  private prover: Prover
  private wallet: Wallet | null = null

  constructor(config: AppConfig, storage: StorageManager) {
    super()
    this.config = config
    this.storage = storage
    this.googleAuth = new GoogleAuth(config)
    this.backend = new Backend(this.config.backendUrl, this.config.backendApiKey)
    this.prover = new Prover(this.config.proverUrl)
  }

  public login(): void {
    try {
      // Generate state for OAuth flow
      const state = this.generateOAuthState()
      this.storage.saveSessionData('oauth_state', state)

      // Redirect to Google OAuth
      const authUrl = this.googleAuth.getAuthUrl(state)
      window.location.href = authUrl
    } catch (error) {
      console.error('Failed to initiate user login:', error)
      throw error
    }
  }

  public async oauthCallback(callbackData: string): Promise<void> {
    try {
      // Parse URL parameters to get authorization code
      const params = new URLSearchParams(callbackData)
      
      const error = params.get('error')
      if (error) {
        throw new Error(`OAuth error: ${error}`)
      }

      const state = params.get('state')
      const savedState = this.storage.getSessionItem('oauth_state')
      this.storage.removeSessionItem('oauth_state')
      if (state !== savedState) {
        throw new Error('State mismatch. Possible CSRF attack')
      }

      const code = params.get('code')
      if (!code) {
        throw new Error('Missing authorization code')
      }

      // Get JWT token using authorization code (now async)
      const jwt = await this.googleAuth.getJWT(code)

      if (!jwt) {
        throw new Error('Failed to get JWT from authorization code')
      }

      // Create initializer without tokenSKey for fresh wallet behavior
      const initialiser = {
        jwt: jwt,
        // No tokenSKey - this tells the Wallet API to treat it as a fresh wallet
      }

      // Create wallet instance
      this.wallet = new Wallet(this.backend, this.prover, initialiser)

      // Check if there is an existing wallet for the same Cardano address
      const address = await this.wallet.getAddress().then((x: any) => x.to_bech32())
      const existingWallet = this.storage.getWallet(address)
      if (existingWallet) {
        // TODO: check if we have a UTxO with the token matching the existing wallet's tokenSKey

        // Reuse existing wallet credential to avoid expensive proof recomputation
        console.log(`Found an existing wallet. Reusing credentials.`)

        this.wallet = existingWallet
        this.wallet.updateBackend(this.backend)
        this.wallet.updateProver(this.prover)
      }
      else {
        // Create new wallet as no existing wallet found for this email
        console.log(`No existing wallet found for the given userId. Creating new wallet.`)
      }

      // Emit wallet initialized event with updated state
      this.emit('walletInitialized')

    } catch (error) {
      console.error('OAuth callback failed:', error)
      throw error
    }

    // Redirect to root address
    window.history.replaceState({}, '', '/')
  }

  public async sendTransaction(request: TransactionRequest): Promise<void> {
    try {
      if (!this.wallet) {
          throw new Error('There is no active wallet when sending transaction')
        }

      console.log(`Sending ${request.amount} ${request.asset} to ${request.recipient} using ${request.recipientType}`)

      // Create asset dictionary
      const assetDict: { [key: string]: BigIntWrap } = {}
      assetDict[request.asset] = new BigIntWrap(request.amount)

      // Create recipient
      let recipient: SmartTxRecipient
      switch (request.recipientType) {
        case AddressType.Bech32:
          recipient = new SmartTxRecipient(AddressType.Bech32, request.recipient, assetDict)
          break
        case AddressType.Email:
          recipient = new SmartTxRecipient(AddressType.Email, request.recipient, assetDict)
          break
        default:
          throw new Error(`Unsupported recipient type: ${request.recipientType}`)
      }

      // Get recipient address for tracking
      let recipientAddress: string
      if (request.recipientType === AddressType.Email) {
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
      const txResponse = await this.wallet.sendTo(recipient)
      const txId = txResponse.transaction_id;
      const failedEmails = txResponse.notifier_errors;
      console.log(`Transaction ID: ${txId}`)
      if (failedEmails && failedEmails.length > 0) {
        console.error('Notifier errors occurred:');
        for (let i = 0; i < failedEmails.length; i++) {
          const failedNotification = failedEmails[i];
          console.error(`Failed to notify recipient ${failedNotification.email}: ${failedNotification.error}`);
        }
      }

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

  public async getUserId(): Promise<string> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized')
    }
    return await this.wallet.getUserId()
  }

  public async getWalletAddress(): Promise<string> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized')
    }
    return await this.wallet.getAddress().then((x: any) => x.to_bech32())
  }

  public async getWalletBalance(): Promise<any> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized')
    }
    return await this.wallet.getBalance()
  }

  public isLoggedIn(): boolean {
    return this.wallet !== null
  }

  public logout(): void {
    // Clear the current session without deleting wallet data from localStorage
    console.log("LOGGING OUT")
    this.wallet = null
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
}
