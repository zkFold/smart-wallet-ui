# zkFold Smart Contract Wallet UI 

This is a demo application for the zkFold Smart Contract Wallet that supports Cardano [CIP-30 API](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0030).

The application uses the `zkfold-smart-wallet-api` npm package to provide wallet functionality.

## Setup

1. Install dependencies:
```bash
npm install
```

2. Configure environment variables:
```bash
cp .env.example .env
```
Then edit `.env` with your actual values:
- `CLIENT_ID` & `CLIENT_SECRET`: Google OAuth credentials
- `WEBSITE_URL`: Base URL for your website (includes protocol, host, and port)
- `BACKEND_URL`: URL for the zkFold Smart Wallet backend API
- `PORT`: Port number for the HTTP server
- `EMAIL_USER` & `EMAIL_KEY`: Email credentials for notifications
- `SESSION_SECRET`: Secure random string for session encryption
- `BACKEND_API_KEY`: API key for backend authentication (optional)

3. Run the application:
```bash
npm start
```

Or for development with auto-reload:
```bash
npm run dev
```
