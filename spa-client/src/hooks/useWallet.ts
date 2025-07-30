import { create } from 'zustand';
import { WalletState, WalletBalance, TransactionRequest } from '../types';
import { walletService } from '../services/walletService';
import { googleAuth } from '../services/googleAuth';
import { backendApi } from '../services/backendApi';

interface WalletStore extends WalletState {
  // Actions
  initializeFromMnemonic: (mnemonic: string, network: string) => Promise<void>;
  initializeFromGoogle: (jwt: string, network: string) => Promise<void>;
  startGoogleAuth: () => { authUrl: string; state: string };
  handleGoogleCallback: (code: string, state: string) => Promise<void>;
  refreshBalance: () => Promise<void>;
  sendTransaction: (request: TransactionRequest) => Promise<string>;
  clearWallet: () => void;
  
  // Loading states
  isLoading: boolean;
  error: string | null;
  setLoading: (loading: boolean) => void;
  setError: (error: string | null) => void;
}

export const useWalletStore = create<WalletStore>((set, get) => ({
  // Initial state
  isInitialized: false,
  balance: null,
  address: null,
  network: 'Preprod',
  isLoading: false,
  error: null,

  // Actions
  initializeFromMnemonic: async (mnemonic: string, network: string) => {
    const { setLoading, setError } = get();
    
    try {
      setLoading(true);
      setError(null);
      
      await walletService.initializeFromMnemonic(mnemonic, network);
      
      const balance = await walletService.getBalance();
      const address = await walletService.getAddress();
      
      set({
        isInitialized: true,
        balance,
        address,
        network: network as 'Preview' | 'Preprod' | 'Mainnet',
        isLoading: false
      });
    } catch (error) {
      setError(error instanceof Error ? error.message : 'Failed to initialize wallet');
      setLoading(false);
      throw error;
    }
  },

  initializeFromGoogle: async (jwt: string, network: string) => {
    const { setLoading, setError } = get();
    
    try {
      setLoading(true);
      setError(null);
      
      await walletService.initializeFromGoogle(jwt, network);
      
      const balance = await walletService.getBalance();
      const address = await walletService.getAddress();
      
      set({
        isInitialized: true,
        balance,
        address,
        network: network as 'Preview' | 'Preprod' | 'Mainnet',
        isLoading: false
      });
    } catch (error) {
      setError(error instanceof Error ? error.message : 'Failed to initialize wallet');
      setLoading(false);
      throw error;
    }
  },

  startGoogleAuth: () => {
    const { setError } = get();
    setError(null);
    return googleAuth.startOAuthFlow();
  },

  handleGoogleCallback: async (code: string, state: string) => {
    const { setLoading, setError, initializeFromGoogle } = get();
    
    try {
      setLoading(true);
      setError(null);
      
      const result = await googleAuth.handleCallback(code, state);
      
      if (result.error) {
        throw new Error(result.error);
      }
      
      // Initialize wallet with the JWT token
      await initializeFromGoogle(result.jwt, 'Preprod'); // Default to Preprod for Google auth
    } catch (error) {
      setError(error instanceof Error ? error.message : 'Failed to handle Google callback');
      setLoading(false);
      throw error;
    }
  },

  refreshBalance: async () => {
    const { setLoading, setError } = get();
    
    if (!walletService.isInitialized()) {
      return;
    }
    
    try {
      setLoading(true);
      setError(null);
      
      const balance = await walletService.getBalance();
      
      set({ balance, isLoading: false });
    } catch (error) {
      setError(error instanceof Error ? error.message : 'Failed to refresh balance');
      setLoading(false);
      throw error;
    }
  },

  sendTransaction: async (request: TransactionRequest) => {
    const { setLoading, setError, refreshBalance } = get();
    
    try {
      setLoading(true);
      setError(null);
      
      const txId = await walletService.sendTransaction(request);
      
      // Send email notification if recipient is Gmail
      if (request.recipientType === 'Gmail') {
        try {
          await backendApi.sendEmailNotification(
            request.recipient,
            request.amount,
            request.asset
          );
        } catch (error) {
          // Log but don't fail the transaction for email errors
          console.warn('Failed to send email notification:', error);
        }
      }
      
      // Refresh balance after successful transaction
      await refreshBalance();
      
      setLoading(false);
      return txId;
    } catch (error) {
      setError(error instanceof Error ? error.message : 'Failed to send transaction');
      setLoading(false);
      throw error;
    }
  },

  clearWallet: () => {
    walletService.clear();
    set({
      isInitialized: false,
      balance: null,
      address: null,
      network: 'Preprod',
      error: null
    });
  },

  setLoading: (loading: boolean) => set({ isLoading: loading }),
  setError: (error: string | null) => set({ error })
}));