import { defineConfig } from 'vite'
import wasm from 'vite-plugin-wasm'
import topLevelAwait from 'vite-plugin-top-level-await'

export default defineConfig({
  plugins: [
    wasm(),
    topLevelAwait()
  ],
  build: {
    outDir: './dist',
    emptyOutDir: true
  },
  server: {
    port: 8080,
    host: true
  },
  define: {
    global: 'globalThis',
  },
  optimizeDeps: {
    include: ['@emurgo/cardano-serialization-lib-browser']
  }
})