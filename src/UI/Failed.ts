import { renderAppHeader } from "./Header"

export function renderFailedView(data: { reason: string }): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container app-container'

  const reason = data?.reason || 'Unknown error'

  container.innerHTML = `
    <section class="app-shell status-shell">
      ${renderAppHeader()}
      <article class="info-card status-card failure">
        <div class="card-header">
          <span class="card-title">Transaction failed</span>
        </div>
        <div class="card-body column">
          <p class="status-message">Something went wrong.</p>
          <p class="status-detail">${reason}</p>
        </div>
        <div class="status-actions">
          <button type="button" id="new_tx" disabled class="primary-action">Make another transaction</button>
          <button type="button" id="logout_button" disabled class="primary-action">Log out</button>
        </div>
      </article>
    </section>
  `

  return container
}