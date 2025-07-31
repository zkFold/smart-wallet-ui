# zkFold Smart Contract Wallet UI 

This is a client-side TypeScript Single Page Application (SPA) for the zkFold Smart Contract Wallet that supports Cardano wallet functionality.

The application has been migrated from a server-side Node.js/Express architecture to a modern client-side TypeScript SPA to resolve compatibility issues with `zkfold-smart-wallet-api` v1.3.0 and improve security and scalability.

## 🚀 Quick Start

1. **Install dependencies:**
```bash
npm install
```

2. **Configure environment variables:**
```bash
cp .env.client.example .env
```
Then edit `.env` with your actual values:
- `VITE_CLIENT_ID`: Google OAuth client ID
- `VITE_WEBSITE_URL`: Base URL for your website (default: http://localhost:3000)
- `VITE_BACKEND_URL`: URL for the zkFold Smart Wallet backend API
- `VITE_BACKEND_API_KEY`: API key for backend authentication (optional)

3. **Run the application:**
```bash
npm run dev
```

This starts both the client-side Vite dev server (port 3000) and a minimal API server (port 3001) for transaction status checking.

## 📁 Project Structure

- **`src/`** - Client-side TypeScript source code
- **`public/`** - Static assets (CSS, images, etc.)
- **`dist/`** - Production build output
- **`api-server.ts`** - Minimal API server for transaction status
- **Legacy files** - Original server-side code preserved for reference

## 🔧 Available Scripts

- `npm run dev` - Start development servers (client + API)
- `npm run dev:client` - Start only the Vite client dev server
- `npm run dev:api` - Start only the API server
- `npm run build` - Build for production
- `npm run preview` - Preview production build
- `npm run legacy:start` - Run original server-side version

## ✨ Features

- **🔐 Dual Authentication**: Google OAuth or mnemonic phrase
- **💳 Multi-Network Support**: Preview and Preprod Cardano networks
- **📧 Email-Based Transactions**: Send ADA to Gmail addresses
- **🏛️ Traditional Addresses**: Support for Bech32 addresses
- **💰 Balance Management**: View wallet balance and assets
- **🔄 Transaction Tracking**: Real-time transaction status updates
- **📱 Responsive Design**: Mobile-friendly interface with Pico CSS
- **🎨 Advanced Controls**: Toggle between simple and advanced modes

## 🔄 Migration from Server-Side

This application was migrated from a server-side Express.js application to a client-side TypeScript SPA. See [MIGRATION.md](./MIGRATION.md) for detailed migration information.

### Key Improvements

- ✅ **Compatibility**: Resolves `zkfold-smart-wallet-api` v1.3.0 browser compatibility issues
- ✅ **Security**: Client-side OAuth flow with PKCE
- ✅ **Scalability**: No server infrastructure required
- ✅ **Performance**: Static hosting and CDN deployment ready
- ✅ **Development**: Modern TypeScript with Vite build system

## 🏗️ Deployment

The application can be deployed to any static hosting service:

1. **Build the application:**
```bash
npm run build
```

2. **Deploy the `dist/` folder** to your static hosting service (Netlify, Vercel, GitHub Pages, etc.)

3. **Deploy the API server** (optional) for transaction status checking:
```bash
# Deploy api-server.ts to a Node.js hosting service
```

## 🔐 Environment Configuration

Client-side environment variables (prefixed with `VITE_`):
- `VITE_CLIENT_ID` - Google OAuth client ID
- `VITE_WEBSITE_URL` - Application base URL  
- `VITE_BACKEND_URL` - zkFold backend API URL
- `VITE_BACKEND_API_KEY` - Backend API key (optional)

## 🧪 Development

The application uses modern web technologies:
- **TypeScript** - Type-safe JavaScript
- **Vite** - Fast build tool and dev server
- **WASM** - WebAssembly support for Cardano libraries
- **localStorage** - Client-side state persistence
- **Event-driven architecture** - Modular component communication

## 📚 Documentation

- [MIGRATION.md](./MIGRATION.md) - Detailed migration guide and technical details
- [Legacy README sections](#legacy-setup) - Original server-side setup (preserved below)

---

## Legacy Setup

<details>
<summary>Original server-side setup instructions (preserved for reference)</summary>

### Original Server-Side Setup

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
- `SESSION_SECRET`: Secure random string for session encryption
- `BACKEND_API_KEY`: API key for backend authentication (optional)

3. Run the legacy server:
```bash
npm run legacy:start
```

</details>
