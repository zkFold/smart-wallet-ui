import { App } from './App'
import { AppConfig } from './Types'
import { Backend, GoogleApi, Prover } from 'zkfold-smart-wallet-api'

// Initialize the application when DOM is loaded
async function initApp() {
  const envProverUrls = import.meta.env.VITE_PROVER_URL
  const proverUrls = envProverUrls
    .split(',')
    .map((url) => url.trim())
    .filter((url) => url.length > 0)

  // Randomize the prover on each load to balance requests across available endpoints
  const selectedProverUrl = proverUrls[Math.floor(Math.random() * proverUrls.length)]

  const config: AppConfig = {
    websiteUrl: import.meta.env.VITE_WEBSITE_URL,
    backendUrl: import.meta.env.VITE_BACKEND_URL,
    backendApiKey: import.meta.env.VITE_BACKEND_API_KEY,
    proverUrl: selectedProverUrl,
  }

  const backend = new Backend(config.backendUrl, config.backendApiKey)
  const prover = new Prover(config.proverUrl)

  const creds = await backend.credentials()
  const googleApi = new GoogleApi(creds.client_id, creds.client_secret, `${config.websiteUrl}/oauth2callback`)

  const app = new App(backend, prover, googleApi)
  app.init()
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initApp)
} else {
  // DOM is already loaded
  initApp()
}
