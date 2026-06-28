import { App } from './App'
import { renderErrorView } from './UI/Error'
import { AppConfig } from './Types'
import { Backend, L2Backend } from 'zkfold-smart-wallet-api'

function requiredEnv(name: string): string {
  const value = import.meta.env[name]
  if (typeof value !== 'string' || value.trim().length === 0) {
    throw new Error(`Missing required environment variable ${name}`)
  }
  return value.trim()
}

function optionalEnv(name: string): string | null {
  const value = import.meta.env[name]
  return typeof value === 'string' && value.trim().length > 0 ? value.trim() : null
}

async function initApp(): Promise<void> {
  try {
    const config: AppConfig = {
      backendUrl: requiredEnv('VITE_BACKEND_URL'),
      backendApiKey: optionalEnv('VITE_BACKEND_API_KEY'),
      rollupUrl: requiredEnv('VITE_ROLLUP_URL'),
      rollupApiKey: optionalEnv('VITE_ROLLUP_API_KEY')
    }

    const backend = new Backend(config.backendUrl, config.backendApiKey)
    const l2Backend = new L2Backend(config.rollupUrl, config.rollupApiKey)

    const app = new App(backend, l2Backend)
    await app.init()
  } catch (error) {
    console.error('Failed to initialize app:', error)
    const app = document.getElementById('app') as HTMLElement
    const viewElement = renderErrorView()
    app.appendChild(viewElement)
  }
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initApp)
} else {
  initApp()
}
