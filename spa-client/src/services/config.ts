import { AppConfig } from '../types';

export const config: AppConfig = {
  backend: {
    url: import.meta.env.VITE_BACKEND_URL || 'http://localhost:8080',
    apiKey: import.meta.env.VITE_BACKEND_API_KEY,
  },
  googleClientId: import.meta.env.VITE_GOOGLE_CLIENT_ID || '',
  websiteUrl: import.meta.env.VITE_WEBSITE_URL || window.location.origin,
};

// Validate required environment variables
const validateConfig = () => {
  if (!config.googleClientId) {
    console.warn('VITE_GOOGLE_CLIENT_ID is not set. Google OAuth will not work.');
  }
  
  if (!config.backend.url) {
    console.warn('VITE_BACKEND_URL is not set. Using default localhost.');
  }
};

validateConfig();