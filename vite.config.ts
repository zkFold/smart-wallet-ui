import { defineConfig } from 'vite'
import wasm from 'vite-plugin-wasm'
import topLevelAwait from 'vite-plugin-top-level-await'
import manifest from './manifest.json'
import { crx } from '@crxjs/vite-plugin'

export default defineConfig({
  plugins: [
    wasm(),
    topLevelAwait(),
    crx({ manifest })
  ],
  build: {
    outDir: './dist',
    emptyOutDir: true,
    chunkSizeWarningLimit: 1500 // Increase from default 500kb to 1.5MB to suppress warning
  },
  server: {
    port: 8080,
    host: true,
    proxy: {
      '/v0': {
        target: 'http://localhost:8082',
        changeOrigin: true,
        secure: false
      }
    },
    cors: {
      origin: "*",     // РАЗРЕШАЕМ ВСЕ (или можно 'chrome-extension://...')
      methods: "GET,HEAD,PUT,PATCH,POST,DELETE",
      preflightContinue: true,
      optionsSuccessStatus: 204
    }
  },
  define: {
    global: 'globalThis',
  },
  optimizeDeps: {
    include: ['@emurgo/cardano-serialization-lib-browser']
  },
  preview: {
    allowedHosts: ['wallet.zkfold.io', 'wallet.zkfold.ch'],
  },
})