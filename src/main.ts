import { App } from './app'

console.log('main.ts loaded')

// Initialize the application when DOM is loaded
function initApp() {
  console.log('Initializing app...')
  try {
    const app = new App()
    app.init()
    console.log('App initialized successfully')
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