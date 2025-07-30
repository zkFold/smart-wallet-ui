import { config } from './config';
import { GoogleAuthResult } from '../types';

export class GoogleAuthService {
  private readonly clientId: string;
  private readonly redirectUri: string;
  private readonly scope: string = 'openid email profile';

  constructor() {
    this.clientId = config.googleClientId;
    this.redirectUri = `${config.websiteUrl}/oauth2callback`;
  }

  /**
   * Generate OAuth2 state parameter for CSRF protection
   */
  generateState(): string {
    const array = new Uint8Array(32);
    crypto.getRandomValues(array);
    return Array.from(array, byte => byte.toString(16).padStart(2, '0')).join('');
  }

  /**
   * Get Google OAuth2 authorization URL
   */
  getAuthUrl(state: string): string {
    const params = new URLSearchParams({
      client_id: this.clientId,
      redirect_uri: this.redirectUri,
      scope: this.scope,
      response_type: 'code',
      state,
      access_type: 'offline',
      prompt: 'consent'
    });

    return `https://accounts.google.com/o/oauth2/v2/auth?${params.toString()}`;
  }

  /**
   * Exchange authorization code for JWT token
   */
  async exchangeCodeForToken(code: string): Promise<GoogleAuthResult> {
    try {
      const tokenResponse = await fetch('https://oauth2.googleapis.com/token', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
        },
        body: new URLSearchParams({
          client_id: this.clientId,
          client_secret: '', // This needs to be handled differently in production
          code,
          grant_type: 'authorization_code',
          redirect_uri: this.redirectUri,
        }),
      });

      if (!tokenResponse.ok) {
        throw new Error(`Token exchange failed: ${tokenResponse.statusText}`);
      }

      const tokenData = await tokenResponse.json();
      
      // Get user info to create JWT-like token
      const userResponse = await fetch('https://www.googleapis.com/oauth2/v2/userinfo', {
        headers: {
          'Authorization': `Bearer ${tokenData.access_token}`,
        },
      });

      if (!userResponse.ok) {
        throw new Error(`User info fetch failed: ${userResponse.statusText}`);
      }

      const userData = await userResponse.json();
      
      // Create a simple JWT-like structure (in production, this should be properly signed)
      const jwt = btoa(JSON.stringify({
        email: userData.email,
        name: userData.name,
        picture: userData.picture,
        sub: userData.id,
        iat: Math.floor(Date.now() / 1000),
        exp: Math.floor(Date.now() / 1000) + 3600, // 1 hour expiry
      }));

      return { jwt };
    } catch (error) {
      console.error('Error exchanging code for token:', error);
      return { jwt: '', error: error instanceof Error ? error.message : 'Unknown error' };
    }
  }

  /**
   * Parse JWT token to get user information
   */
  parseJWT(jwt: string): any {
    try {
      return JSON.parse(atob(jwt));
    } catch (error) {
      console.error('Error parsing JWT:', error);
      return null;
    }
  }

  /**
   * Check if JWT token is valid
   */
  isTokenValid(jwt: string): boolean {
    try {
      const payload = this.parseJWT(jwt);
      if (!payload) return false;
      
      const now = Math.floor(Date.now() / 1000);
      return payload.exp > now;
    } catch (error) {
      return false;
    }
  }

  /**
   * Start OAuth flow
   */
  startOAuthFlow(): { authUrl: string; state: string } {
    const state = this.generateState();
    const authUrl = this.getAuthUrl(state);
    
    // Store state in sessionStorage for verification
    sessionStorage.setItem('oauth_state', state);
    
    return { authUrl, state };
  }

  /**
   * Handle OAuth callback
   */
  async handleCallback(code: string, state: string): Promise<GoogleAuthResult> {
    // Verify state parameter
    const storedState = sessionStorage.getItem('oauth_state');
    if (state !== storedState) {
      return { jwt: '', error: 'State mismatch. Possible CSRF attack.' };
    }

    // Clear stored state
    sessionStorage.removeItem('oauth_state');

    // Exchange code for token
    return this.exchangeCodeForToken(code);
  }
}

export const googleAuth = new GoogleAuthService();