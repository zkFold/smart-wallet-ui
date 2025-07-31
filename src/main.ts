import { App } from './app'

// Initialize the application when DOM is loaded
function initApp() {
  try {
    const app = new App()
    app.init()
  } catch (error) {
    console.error('Failed to initialize app:', error)
  }
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initApp)
} else {
  // DOM is already loaded
  initApp()
}