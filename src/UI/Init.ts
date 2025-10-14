export function renderInitView(): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container app-container'

  container.innerHTML = `
    <section class="app-shell intro-shell">
  <header class="app-header app-header--centered">
        <div class="app-brand">
          <a href="https://zkfold.io" class="app-logo-link">
            <img src="logo-200x73.png" alt="zkFold logo" class="app-logo">
          </a>
          <div class="app-brand-copy">
            <h1>Smart Wallet</h1>
            <p>Your gateway to crypto and Web3</p>
          </div>
        </div>
      </header>
      <div class="auth-shell">
        <form action="#" method="POST" class="info-card auth-card auth-card--centered">
          <div class="card-header">
            <span class="card-title">Sign in</span>
          </div>
          <div class="card-body column">
            <p class="card-description">Continue with Google to access your zkFold Smart Wallet.</p>
            <button
              id="submit"
              type="submit"
              class="primary-action primary-action--google"
            >
              <svg width="26" height="26" viewBox="0 0 18 18" xmlns="http://www.w3.org/2000/svg" aria-hidden="true">
                <g fill="none" fill-rule="evenodd">
                  <path d="M17.64 9.205c0-.639-.057-1.252-.164-1.841H9v3.481h4.844a4.14 4.14 0 0 1-1.796 2.716v2.259h2.908c1.702-1.567 2.684-3.875 2.684-6.615z" fill="#4285F4"/>
                  <path d="M9 18c2.43 0 4.467-.806 5.956-2.18l-2.908-2.259c-.806.54-1.837.86-3.048.86-2.344 0-4.328-1.584-5.036-3.711H.957v2.332A8.997 8.997 0 0 0 9 18z" fill="#34A853"/>
                  <path d="M3.964 10.71A5.41 5.41 0 0 1 3.682 9c0-.593.102-1.17.282-1.71V4.958H.957A8.996 8.996 0 0 0 0 9c0 1.452.348 2.827.957 4.042l3.007-2.332z" fill="#FBBC05"/>
                  <path d="M9 3.58c1.321 0 2.508.454 3.44 1.345l2.582-2.58C13.463.891 11.426 0 9 0A8.997 8.997 0 0 0 .957 4.958L3.964 7.29C4.672 5.163 6.656 3.58 9 3.58z" fill="#EA4335"/>
                </g>
              </svg>
              Continue with Google
            </button>
          </div>
        </form>
      </div>
    </section>
  `

  return container
}