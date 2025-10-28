# zkFold Smart Wallet UI 

This is a demo UI for the Smart Wallet API library. It illustrates one can use Smart Wallet API to build a Google OAuth-based wallet on Cardano.

## 🚀 Quick Start

1. **Install dependencies:**
```bash
npm install
```

2. **Configure environment variables:**
Edit `.env`:
- `VITE_CLIENT_ID`: Google OAuth client ID
- `VITE_CLIENT_SECRET`: Google OAuth client secret
- `VITE_WEBSITE_URL`: Base URL for your website (default: http://localhost:8080)
- `VITE_BACKEND_URL`: URL for the zkFold Smart Wallet backend server
- `VITE_BACKEND_API_KEY`: an API key to access the Smart Wallet backend server
- `VITE_PROVER_URL`: Comma-separated list of prover server URLs. One will be selected at random on each load.

3. **Run the application:**
```bash
npm run dev
```

The application runs on http://localhost:8080.

## 🏗️ Deployment

The application can be deployed to any static hosting service:

1. **Build the application:**
```bash
npm run build
```

2. **Deploy the `dist/` folder** to your static hosting service.
