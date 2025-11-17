import { renderAppFrame } from "./Frame"

export function renderErrorView(): HTMLElement {
  const content = `
    <h1 class="text_center">Service temporarily unavailable</h1>
    <p class="sub_title text_center">An error has occurred on the server. Please, contact info@zkfold.io for support. </p>
  `

  return renderAppFrame(content, false)
}
