import axios, { AxiosInstance } from 'axios';
import { config } from './config';
import { TransactionResult, WalletBalance } from '../types';

export class BackendApiService {
  private api: AxiosInstance;

  constructor() {
    this.api = axios.create({
      baseURL: config.backend.url,
      headers: {
        'Content-Type': 'application/json',
        ...(config.backend.apiKey && { 'Authorization': `Bearer ${config.backend.apiKey}` }),
      },
      timeout: 30000, // 30 second timeout
      withCredentials: true, // Important for session cookies
    });

    // Add request/response interceptors for logging
    this.api.interceptors.request.use(
      (config) => {
        console.log(`API Request: ${config.method?.toUpperCase()} ${config.url}`);
        return config;
      },
      (error) => {
        console.error('API Request Error:', error);
        return Promise.reject(error);
      }
    );

    this.api.interceptors.response.use(
      (response) => {
        console.log(`API Response: ${response.status} ${response.config.url}`);
        return response;
      },
      (error) => {
        console.error('API Response Error:', error);
        return Promise.reject(error);
      }
    );
  }

  /**
   * Initialize wallet session on backend
   */
  async initializeWallet(data: {
    method: string;
    network: string;
    address: string;
    jwt?: string;
  }): Promise<{ success: boolean; error?: string }> {
    try {
      const response = await this.api.post('/api/init-session', {
        method: data.method,
        network: data.network,
        address: data.address,
        ...(data.jwt && { jwt: data.jwt })
      });
      return { success: true };
    } catch (error) {
      console.error('Error initializing wallet session:', error);
      return { success: false, error: 'Failed to initialize wallet session' };
    }
  }

  /**
   * Get wallet balance
   */
  async getBalance(address: string): Promise<WalletBalance> {
    try {
      const response = await this.api.get('/api/balance', {
        params: { address }
      });
      return response.data;
    } catch (error) {
      console.error('Error getting balance:', error);
      return { lovelace: '0' };
    }
  }

  /**
   * Get Gmail address
   */
  async getGmailAddress(email: string): Promise<string> {
    try {
      const response = await this.api.get('/api/gmail-address', {
        params: { email }
      });
      return response.data.address;
    } catch (error) {
      console.error('Error getting Gmail address:', error);
      return email; // Fallback to email
    }
  }

  /**
   * Send transaction
   */
  async sendTransaction(data: {
    fromAddress: string;
    toAddress: string;
    recipientType: 'Gmail' | 'Bech32';
    asset: string;
    amount: string;
  }): Promise<string> {
    try {
      const response = await this.api.post('/api/send-tx', {
        fromAddress: data.fromAddress,
        toAddress: data.toAddress,
        recipientType: data.recipientType,
        asset: data.asset,
        amount: data.amount
      });
      return response.data.txId;
    } catch (error) {
      console.error('Error sending transaction:', error);
      throw new Error('Failed to send transaction');
    }
  }

  /**
   * Check transaction status
   */
  async checkTransactionStatus(txId: string, recipient: string): Promise<TransactionResult> {
    try {
      const response = await this.api.get('/tx_status', {
        params: { txId, recipient }
      });
      return response.data;
    } catch (error) {
      console.error('Error checking transaction status:', error);
      return { outcome: 'failure', reason: 'Failed to check transaction status' };
    }
  }

  /**
   * Send email notification (if recipient is Gmail)
   */
  async sendEmailNotification(recipient: string, amount: string, asset: string): Promise<void> {
    try {
      await this.api.post('/send-notification', {
        recipient,
        amount,
        asset,
        websiteUrl: config.websiteUrl
      });
    } catch (error) {
      console.error('Error sending email notification:', error);
      // Don't throw error for email notifications - it's not critical
    }
  }

  /**
   * Health check
   */
  async healthCheck(): Promise<boolean> {
    try {
      const response = await this.api.get('/health');
      return response.status === 200;
    } catch (error) {
      console.error('Backend health check failed:', error);
      return false;
    }
  }
}

export const backendApi = new BackendApiService();