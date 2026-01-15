import { App } from './App'
import { renderErrorView } from './UI/Error'

// Initialize the application when DOM is loaded
async function initApp() {
  try {
    const app = await App.createFromEnv()
    app.init()
  } catch {
    const app = document.getElementById('app') as HTMLElement
    const viewElement = renderErrorView()
    app.appendChild(viewElement)
  }
}

// TODO: we need a `try` here in case the backend is down
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initApp)
} else {
  // DOM is already loaded
  initApp()
}
