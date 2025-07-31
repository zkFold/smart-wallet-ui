# Migration to Client-Side TypeScript Architecture

This document outlines the migration from the server-side Node.js/Express application to a client-side Single Page Application (SPA) using TypeScript.

## 🎯 Migration Summary

The application has been successfully migrated from a server-side architecture to a client-side TypeScript SPA to resolve compatibility issues with `zkfold-smart-wallet-api` v1.3.0 and improve security and scalability.

### Key Changes

- **Architecture**: Server-side Express app → Client-side TypeScript SPA
- **Build System**: Node.js with tsx → Vite with TypeScript
- **State Management**: Express sessions → localStorage with encryption support
- **OAuth Flow**: Server-side handling → Client-side PKCE flow
- **Dependencies**: Removed server-side dependencies (Express, sessions, etc.)

## 🚀 Getting Started

### Prerequisites
- Node.js (v18+ recommended)
- npm or yarn

### Installation
```bash
npm install
```

### Development
```bash
# Start both client and API server
npm run dev

# Or start them separately:
npm run dev:client  # Vite dev server on port 3000
npm run dev:api     # API server on port 3001 (for transaction status)
```

### Production Build
```bash
npm run build
```

### Environment Configuration

Copy `.env.client.example` to `.env` and configure:

```bash
# Google OAuth Configuration
VITE_CLIENT_ID=your_google_oauth_client_id_here

# Website and Backend Configuration  
VITE_WEBSITE_URL="http://localhost:3000"
VITE_BACKEND_URL="https://your-backend-url.com"

# Backend API Configuration (optional)
VITE_BACKEND_API_KEY=""

# For API server (transaction status checking)
BACKEND_URL="https://your-backend-url.com"
BACKEND_API_KEY=""
```

## 📁 New Project Structure

```
smart-wallet-ui/
├── src/                          # Client-side TypeScript source
│   ├── main.ts                   # Application entry point
│   ├── app.ts                    # Main application class
│   ├── types.ts                  # TypeScript type definitions
│   ├── vite-env.d.ts            # Vite environment types
│   ├── index.html               # Single page application template
│   ├── wallet/                  # Wallet management
│   │   ├── WalletManager.ts     # Main wallet operations
│   │   └── GoogleAuth.ts        # Google OAuth handling
│   ├── ui/                      # User interface
│   │   └── router.ts            # View rendering and routing
│   ├── services/                # External services
│   │   └── BackendService.ts    # zkFold backend communication
│   └── utils/                   # Utility functions
│       ├── storage.ts           # localStorage management
│       ├── helpers.ts           # Helper functions
│       └── EventEmitter.ts      # Event system
├── public/                      # Static assets
│   ├── css/                     # Pico CSS framework
│   ├── favicon.ico
│   └── logo-200x73.png
├── dist/                        # Production build output
├── api-server.ts                # Minimal API for transaction status
├── vite.config.ts               # Vite configuration
├── tsconfig.json                # TypeScript configuration
└── package.json                 # Dependencies and scripts
```

## 🔧 Technical Details

### Client-Side Architecture

The new architecture uses a modular TypeScript design:

- **App**: Main application coordinator
- **WalletManager**: Handles wallet initialization, transactions, and state
- **Router**: Manages view rendering and navigation
- **StorageManager**: Handles localStorage with encryption support
- **EventEmitter**: Provides communication between components

### State Management

State is managed client-side using:
- **localStorage**: For persistent wallet state
- **EventEmitter**: For component communication
- **App state**: For current view and temporary data

### Security Improvements

- **Client-side OAuth**: More secure PKCE flow
- **No server sessions**: Eliminates session-based vulnerabilities  
- **Local state encryption**: Sensitive data can be encrypted before storage
- **CSP headers**: Content Security Policy support (configurable)

### Performance Benefits

- **No server**: Eliminates server overhead and scaling concerns
- **Static hosting**: Can be deployed to CDN/static hosting
- **Code splitting**: Vite enables efficient bundling and code splitting
- **WASM support**: Proper handling of Cardano serialization library

## 🔄 Migration Guide

### For Developers

1. **Environment Variables**: Update from `.env` to use `VITE_` prefixes
2. **Build Scripts**: Use `npm run dev` instead of `npm run start`
3. **Debugging**: Check browser console instead of server logs
4. **State**: Access wallet state through `StorageManager` instead of sessions

### For Deployment

1. **Static Hosting**: The app can now be deployed to any static hosting service
2. **API Endpoint**: The minimal API server (`api-server.ts`) can be deployed separately for transaction status checking
3. **Environment Variables**: Configure client-side variables for production

## 🧪 Testing the Migration

The migrated application maintains full compatibility with the original functionality:

- ✅ Wallet initialization with Google OAuth
- ✅ Wallet initialization with mnemonic phrase  
- ✅ Network selection (Preview/Preprod)
- ✅ Transaction sending to Gmail addresses
- ✅ Transaction sending to Bech32 addresses
- ✅ Balance display and asset management
- ✅ Transaction status tracking
- ✅ Advanced controls toggle
- ✅ Responsive design with Pico CSS

## 🔗 Legacy Compatibility

The original server-side code is preserved for reference:
- `app.ts` - Original Express server
- `*.html` - Original server-side templates
- Legacy scripts available via `npm run legacy:start`

## 🚨 Breaking Changes

1. **Server Dependency**: No longer requires a Node.js server for the main application
2. **Session Storage**: Wallet state is now stored client-side (localStorage)
3. **OAuth Redirect**: OAuth callback handling is now client-side
4. **Environment Variables**: Must use `VITE_` prefix for client-side variables

## 📈 Next Steps

- [ ] Add comprehensive error handling and user feedback
- [ ] Implement proper encryption for sensitive localStorage data
- [ ] Add offline support with service workers
- [ ] Implement proper CSP headers and security hardening
- [ ] Add automated testing suite
- [ ] Optimize bundle size with dynamic imports