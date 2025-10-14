export function renderFailedView(data: { reason: string }): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container app-container'

  const reason = data?.reason || 'Unknown error'

  container.innerHTML = `
    <section class="app-shell status-shell">
      <header class="app-header">
        <div class="app-brand">
          <a href="https://zkfold.io" class="app-logo-link">
            <img src="logo-200x73.png" alt="zkFold logo" class="app-logo">
          </a>
          <div class="app-brand-copy">
            <h1>Something went wrong</h1>
            <p>We couldn’t complete your transaction. Review the details below and try again.</p>
          </div>
        </div>
      </header>
      <article class="info-card status-card failure">
        <div class="card-header">
          <span class="card-title">Transaction failed</span>
        </div>
        <div class="card-body column">
          <p class="status-message">We hit a snag.</p>
          <p class="status-detail">${reason}</p>
        </div>
        <div class="status-actions">
          <button id="new_tx" disabled class="primary-action">Make another transaction</button>
          <button id="new_wallet" disabled class="ghost-button">Log out</button>
        </div>
      </article>
    </section>
  `

  return container
}