import { App } from './App'

// Initialize the application when DOM is loaded
function initApp() {
  try {
    const app = new App()
    app.init("zkFold")
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
