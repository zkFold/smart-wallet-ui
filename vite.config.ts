import { defineConfig } from 'vite'
import wasm from 'vite-plugin-wasm'
import topLevelAwait from 'vite-plugin-top-level-await'

export default defineConfig({
  base: '/smart-wallet-ui/',
  plugins: [
    wasm(),
    topLevelAwait()
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
    }
  },
  define: {
    global: 'globalThis',
  },
  optimizeDeps: {
    include: ['@emurgo/cardano-serialization-lib-browser']
  }
})