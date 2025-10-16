import { App } from './App'

// Initialize the application when DOM is loaded
function initApp() {
  const app = new App()
  app.init()
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initApp)
} else {
  // DOM is already loaded
  initApp()
}
