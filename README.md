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
- `REDIRECT_URL`: OAuth callback URL for your deployment
- `EMAIL_USER` & `EMAIL_KEY`: Email credentials for notifications
- `SESSION_SECRET`: Secure random string for session encryption
- `HOST`, `PORT`, `PROTOCOL`: Server configuration

3. Run the application:
```bash
npm start
```

Or for development with auto-reload:
```bash
npm run dev
```

The server will start on port 8080 by default.

## Security Notes

- Never commit `.env` files to version control
- Regenerate all secrets before production deployment
- Use proper secrets management in production environments
