export function renderAppFrame(content: string, showLogout: boolean): HTMLElement {
  const container = document.createElement('main')
  container.className = 'container app-container'

  const logoutButtonClass = showLogout ? 'active' : ''

  container.innerHTML = `
    <section class="wallet_sec">
      <div class="wallet_container">
        <form class="wallet_form">
          <div class="form_header">
            <div class="form_col">
              <img class="form_logo" src="logo.svg" alt="zkFold logo">
            </div>
            <div class="form_col btn_col">
              <button id="logout_button" type="button" class="wallet_btn ${logoutButtonClass}">
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-log-out h-4 w-4" data-lov-id="src/pages/Dashboard.tsx:88:16" data-lov-name="LogOut" data-component-path="src/pages/Dashboard.tsx" data-component-line="88" data-component-file="Dashboard.tsx" data-component-name="LogOut" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"></path><polyline points="16 17 21 12 16 7"></polyline><line x1="21" x2="9" y1="12" y2="12"></line></svg>
              </button>
            </div>
          </div>
          <div class="wallet_box_cont">
		  	${content}
          </div>
        </form>
      </div>
    </section>
	<div id="notification" class="notice_box">
      <h3 id="notification_header" class="notice_head">Copied!</h3>
      <p id="notification_body" class="notice_body">Email copied to clipboard</p>
      <input id="notification_timeout_id" type="hidden">
      <svg id="notification_close_icon" class="close_icon" xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-x h-4 w-4" data-lov-id="src/components/ui/toast.tsx:76:4" data-lov-name="X" data-component-path="src/components/ui/toast.tsx" data-component-line="76" data-component-file="toast.tsx" data-component-name="X" data-component-content="%7B%22className%22%3A%22h-4%20w-4%22%7D"><path d="M18 6 6 18"></path><path d="m6 6 12 12"></path></svg>
    </div>
  `

  return container
}