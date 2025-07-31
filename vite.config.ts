import { defineConfig } from 'vite'
import wasm from 'vite-plugin-wasm'
import topLevelAwait from 'vite-plugin-top-level-await'

export default defineConfig({
  root: './src',
  publicDir: '../public',
  plugins: [
    wasm(),
    topLevelAwait()
  ],
  build: {
    outDir: '../dist',
    emptyOutDir: true,
    rollupOptions: {
      input: {
        main: './src/index.html'
      }
    }
  },
  server: {
    port: 3000,
    host: true,
    proxy: {
      '/tx_status': {
        target: 'http://localhost:3001',
        changeOrigin: true
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