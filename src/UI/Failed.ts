export function renderFailedView(data: { reason: string }): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container'

  const reason = data?.reason || 'Unknown error'

  container.innerHTML = `
    <a href="https://zkfold.io">
      <img src="logo-200x73.png" style="width:250px;height:100px;">
    </a>
    <br><br>
    <h1>Transaction failed.</h1>
    <label name="balance_label">
        Reason: ${reason}
    </label>
    <button id="new_tx" disabled>Make another transaction</button>
    <button id="new_wallet" disabled>Log out</button>
  `

  return container
}