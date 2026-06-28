import { renderAppFrame } from './Frame'

export function renderInitView(): HTMLElement {
  const content = `
    <h1 class="text_center">zkFold Rollup</h1>
    <form id="seedphrase_form" class="form_fields">
      <div class="form_field_cont">
        <label for="seedphrase_input">Seed phrase</label>
        <textarea
          id="seedphrase_input"
          name="seedphrase"
          class="input_field seedphrase_input"
          autocomplete="off"
          spellcheck="false"
          required
        ></textarea>
      </div>
      <button
        id="seedphrase_submit"
        type="submit"
        class="submit_btn"
      >
        Open wallet
      </button>
    </form>
  `

  return renderAppFrame(content, false)
}
