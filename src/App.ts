import { AppView, DemoAction, WalletViewModel } from './Types'
import { renderInitView } from './UI/Init'
import { renderWalletView } from './UI/Wallet'
import { renderErrorView } from './UI/Error'
import {
  AddressType,
  Backend,
  L2Backend,
  L2Wallet,
  SeedphraseWallet
} from 'zkfold-smart-wallet-api'
import type { BalanceResponse, Transaction, UTxO } from 'zkfold-smart-wallet-api'
import { AssetMetadataMap, buildAssetMetadata } from './Utils/Assets'

type WalletAction = () => Promise<void>

export class App {
  private l1Wallet: SeedphraseWallet | null = null
  private l2Wallet: L2Wallet | null = null
  private balanceRefreshInterval: number | null = null
  private actions: DemoAction[] = []
  private currentModel: WalletViewModel | null = null

  constructor(
    private readonly backend: Backend,
    private readonly l2Backend: L2Backend
  ) {}

  public async init(): Promise<void> {
    await this.render('init')
  }

  private async render(view: AppView): Promise<void> {
    const app = document.getElementById('app') as HTMLElement
    app.innerHTML = ''
    this.clearBalanceRefreshInterval()

    let viewElement: HTMLElement
    switch (view) {
      case 'wallet':
        this.currentModel = await this.loadWalletModel()
        viewElement = renderWalletView(this.currentModel)
        app.appendChild(viewElement)
        this.setupWalletHandlers()
        this.balanceRefreshInterval = window.setInterval(() => {
          void this.refreshWalletView()
        }, 15_000)
        break
      case 'error':
        viewElement = renderErrorView()
        app.appendChild(viewElement)
        break
      default:
        viewElement = renderInitView()
        app.appendChild(viewElement)
        this.setupInitHandlers()
    }
  }

  private setupInitHandlers(): void {
    const form = document.getElementById('seedphrase_form') as HTMLFormElement | null
    const seedphraseInput = document.getElementById('seedphrase_input') as HTMLTextAreaElement | null
    const submitButton = document.getElementById('seedphrase_submit') as HTMLButtonElement | null

    if (!form || !seedphraseInput || !submitButton) {
      return
    }

    form.addEventListener('submit', async (event) => {
      event.preventDefault()
      const seedphrase = seedphraseInput.value.trim().replace(/\s+/g, ' ')
      if (!seedphrase) {
        this.showNotification('Missing seed phrase', 'Enter the wallet seed phrase.', 'error')
        return
      }

      this.setButtonLoading(submitButton, true, 'Opening wallet...')
      try {
        const l1Wallet = new SeedphraseWallet(this.backend, seedphrase)
        await l1Wallet.setNetwork()

        const l2Wallet = new L2Wallet(this.backend, this.l2Backend, seedphrase)
        await l2Wallet.setNetwork()

        this.l1Wallet = l1Wallet
        this.l2Wallet = l2Wallet
        this.actions = []
        await this.render('wallet')
      } catch (error) {
        const message = this.errorMessage(error)
        console.error('Failed to open seed phrase wallet:', error)
        this.showNotification('Failed to open wallet', message, 'error')
      } finally {
        this.setButtonLoading(submitButton, false)
      }
    })
  }

  private setupWalletHandlers(): void {
    const model = this.currentModel
    if (!model) {
      return
    }

    this.bindCopyButton('copy_l1_address', model.l1Address, 'L1 address copied.')
    this.bindCopyButton('copy_l2_address', model.l2Address, 'L2 address copied.')

    const logoutBtn = document.getElementById('logout_button')
    if (logoutBtn) {
      logoutBtn.addEventListener('click', async () => {
        this.l1Wallet = null
        this.l2Wallet = null
        this.actions = []
        this.currentModel = null
        await this.render('init')
      })
    }

    for (const header of Array.from(document.querySelectorAll('.wallet_sec .wallet_box .wallet_box_header'))) {
      header.addEventListener('click', () => {
        const walletBox = header.closest('.wallet_box')
        walletBox?.classList.toggle('active')
      })
    }

    this.bindExpandableList('l1_transactions_list')
    this.bindExpandableList('l2_transactions_list')
    this.bindExpandableList('wallet_actions_list')

    this.bindBridgeInForm()
    this.bindL2TransferForm()
    this.bindBridgeOutForm()
    this.bindNotificationClose()
  }

  private bindBridgeInForm(): void {
    const form = document.getElementById('bridge_in_form') as HTMLFormElement | null
    if (!form) return

    form.addEventListener('submit', async (event) => {
      event.preventDefault()
      try {
        const model = this.requireModel()
        const metadata = buildAssetMetadata(model.l1Balance)
        const assets = this.assetsFromForm(form, 'bridge_in_amount', 'bridge_in_asset', metadata)

        await this.runWalletAction(form, 'Bridge in', 'Bridge-in transaction submitted.', async () => {
          const l2Wallet = this.requireL2Wallet()
          l2Wallet.setL2Mode(false)
          await l2Wallet.sendTransaction({
            recipient: model.l2Address,
            recipientType: AddressType.L2,
            assets
          })
        })
      } catch (error) {
        this.showNotification('Invalid input', this.errorMessage(error), 'error')
      }
    })
  }

  private bindL2TransferForm(): void {
    const form = document.getElementById('l2_transfer_form') as HTMLFormElement | null
    if (!form) return

    form.addEventListener('submit', async (event) => {
      event.preventDefault()
      try {
        const model = this.requireModel()
        const metadata = buildAssetMetadata(model.l2Balance)
        const assets = this.assetsFromForm(form, 'l2_transfer_amount', 'l2_transfer_asset', metadata)
        const recipient = this.requiredFormValue(form, 'l2_transfer_recipient')

        await this.runWalletAction(form, 'L2 transfer', 'L2 transaction submitted.', async () => {
          const l2Wallet = this.requireL2Wallet()
          l2Wallet.setL2Mode(true)
          await l2Wallet.sendTransaction({
            recipient,
            recipientType: AddressType.L2,
            assets
          })
        })
      } catch (error) {
        this.showNotification('Invalid input', this.errorMessage(error), 'error')
      }
    })
  }

  private bindBridgeOutForm(): void {
    const form = document.getElementById('bridge_out_form') as HTMLFormElement | null
    if (!form) return

    form.addEventListener('submit', async (event) => {
      event.preventDefault()
      try {
        const model = this.requireModel()
        const metadata = buildAssetMetadata(model.l2Balance)
        const assets = this.assetsFromForm(form, 'bridge_out_amount', 'bridge_out_asset', metadata)
        const recipient = this.requiredFormValue(form, 'bridge_out_recipient')

        await this.runWalletAction(form, 'Bridge out', 'Bridge-out transaction submitted.', async () => {
          const l2Wallet = this.requireL2Wallet()
          l2Wallet.setL2Mode(true)
          await l2Wallet.sendTransaction({
            recipient,
            recipientType: AddressType.Bech32,
            assets
          })
        })
      } catch (error) {
        this.showNotification('Invalid input', this.errorMessage(error), 'error')
      }
    })
  }

  private async runWalletAction(
    form: HTMLFormElement,
    label: string,
    successDetail: string,
    action: WalletAction
  ): Promise<void> {
    const button = form.querySelector('button[type="submit"]') as HTMLButtonElement | null
    const actionId = this.addAction(label, 'Submitting...')
    this.setButtonLoading(button, true)

    try {
      await action()
      this.updateAction(actionId, 'success', successDetail)
      form.reset()
      this.showNotification('Submitted', successDetail, 'success')
      await this.render('wallet')
    } catch (error) {
      const message = this.errorMessage(error)
      this.updateAction(actionId, 'error', message)
      console.error(`${label} failed:`, error)
      this.showNotification('Failed', message, 'error')
      await this.render('wallet')
    } finally {
      this.setButtonLoading(button, false)
    }
  }

  private async loadWalletModel(): Promise<WalletViewModel> {
    const l1Wallet = this.requireL1Wallet()
    const l2Wallet = this.requireL2Wallet()
    const previousL2Mode = l2Wallet.getL2Mode()

    const l1Address = await l1Wallet.stringAddress()
    const l2Address = l2Wallet.l2Address().toString()

    const [l1Balance, l1Transactions] = await Promise.all([
      this.loadL1Balance(l1Wallet),
      this.loadTransactions(() => l1Wallet.getTxHistory())
    ])

    l2Wallet.setL2Mode(true)
    try {
      const [l2Balance, l2Transactions] = await Promise.all([
        this.loadBalance(() => l2Wallet.getBalance(), 'L2'),
        this.loadTransactions(() => l2Wallet.getTxHistory())
      ])

      return {
        l1Address,
        l2Address,
        l1Balance,
        l2Balance,
        l1Transactions,
        l2Transactions,
        actions: [...this.actions]
      }
    } finally {
      l2Wallet.setL2Mode(previousL2Mode)
    }
  }

  private async loadTransactions(load: () => Promise<Transaction[]>): Promise<Transaction[]> {
    try {
      return await load()
    } catch (error) {
      console.log('Transaction history could not be fetched:', error)
      return []
    }
  }

  private async loadBalance(load: () => Promise<BalanceResponse>, label: string): Promise<BalanceResponse> {
    try {
      return await load()
    } catch (error) {
      console.log(`${label} balance could not be fetched:`, error)
      return { lovelace: 0, tokens: [], usd: 0 }
    }
  }

  private async loadL1Balance(wallet: SeedphraseWallet): Promise<BalanceResponse> {
    try {
      return await wallet.getBalance()
    } catch (error) {
      console.log('L1 balance endpoint could not be fetched, using UTxO fallback:', error)
      return this.balanceFromUtxos(await wallet.getUtxos())
    }
  }

  private balanceFromUtxos(utxos: UTxO[]): BalanceResponse {
    const lovelace = utxos.reduce((total, utxo) => {
      return total + (utxo.value.lovelace?.toNumber() ?? 0)
    }, 0)

    return { lovelace, tokens: [], usd: 0 }
  }

  private async refreshWalletView(): Promise<void> {
    const activeElement = document.activeElement
    const isEditing = activeElement instanceof HTMLInputElement
      || activeElement instanceof HTMLTextAreaElement
      || activeElement instanceof HTMLSelectElement

    if (isEditing || !this.l1Wallet || !this.l2Wallet) {
      return
    }

    try {
      await this.render('wallet')
    } catch (error) {
      console.error('Failed to refresh wallet view:', error)
    }
  }

  private assetsFromForm(
    form: HTMLFormElement,
    amountField: string,
    assetField: string,
    metadata: AssetMetadataMap
  ): { [asset: string]: number } {
    const amountText = this.requiredFormValue(form, amountField)
    const asset = this.requiredFormValue(form, assetField)
    const amount = Number(amountText)
    if (!Number.isFinite(amount) || amount <= 0) {
      throw new Error('Enter a positive amount.')
    }

    const decimals = metadata[asset]?.decimals ?? 0
    return { [asset]: Math.round(amount * 10 ** decimals) }
  }

  private requiredFormValue(form: HTMLFormElement, name: string): string {
    const value = new FormData(form).get(name)
    if (typeof value !== 'string' || value.trim().length === 0) {
      throw new Error('Fill in all required fields.')
    }
    return value.trim()
  }

  private addAction(label: string, detail: string): string {
    const id = `${Date.now()}-${Math.random().toString(16).slice(2)}`
    this.actions.unshift({
      id,
      label,
      detail,
      status: 'pending',
      timestamp: new Date().toISOString()
    })
    this.actions = this.actions.slice(0, 8)
    return id
  }

  private updateAction(id: string, status: DemoAction['status'], detail: string): void {
    this.actions = this.actions.map((action) => {
      if (action.id !== id) {
        return action
      }
      return {
        ...action,
        status,
        detail,
        timestamp: new Date().toISOString()
      }
    })
  }

  private bindCopyButton(id: string, value: string, message: string): void {
    const button = document.getElementById(id)
    if (!button) return

    button.addEventListener('click', async () => {
      await navigator.clipboard.writeText(value)
      this.showNotification('Copied', message, 'info')
    })
  }

  private bindExpandableList(id: string): void {
    const list = document.getElementById(id)
    if (!list) return

    list.addEventListener('click', (event) => {
      const target = event.target
      if (!(target instanceof Element)) return

      const button = target.closest('.wallet_detail_list__item-btn')
      if (!button || !list.contains(button)) return

      const item = button.closest('.wallet_detail_list__item')
      const content = item?.querySelector('.wallet_detail_list__details') as HTMLElement | null
      if (content) {
        content.style.display = content.style.display === 'none' ? 'block' : 'none'
      }
    })
  }

  private bindNotificationClose(): void {
    const copyCloseIcon = document.getElementById('notification_close_icon')
    if (copyCloseIcon) {
      copyCloseIcon.addEventListener('click', () => {
        const notification = document.getElementById('notification')
        notification?.classList.remove('active')
      })
    }
  }

  private requireL1Wallet(): SeedphraseWallet {
    if (!this.l1Wallet) {
      throw new Error('L1 wallet is not initialized.')
    }
    return this.l1Wallet
  }

  private requireL2Wallet(): L2Wallet {
    if (!this.l2Wallet) {
      throw new Error('L2 wallet is not initialized.')
    }
    return this.l2Wallet
  }

  private requireModel(): WalletViewModel {
    if (!this.currentModel) {
      throw new Error('Wallet state is not loaded.')
    }
    return this.currentModel
  }

  private clearBalanceRefreshInterval(): void {
    if (this.balanceRefreshInterval !== null) {
      clearInterval(this.balanceRefreshInterval)
      this.balanceRefreshInterval = null
    }
  }

  private setButtonLoading(button: HTMLButtonElement | null, loading: boolean, label?: string): void {
    if (!button) return
    button.disabled = loading
    button.classList.toggle('loading', loading)
    if (loading) {
      if (!button.dataset.label) {
        button.dataset.label = (button.textContent || '').trim()
      }
      button.textContent = label ?? 'Submitting...'
    } else {
      button.textContent = button.dataset.label ?? 'Submit'
      delete button.dataset.label
    }
  }

  private errorMessage(error: unknown): string {
    return error instanceof Error ? error.message : String(error)
  }

  private showNotification(header: string, body: string, type: 'info' | 'success' | 'error' = 'info'): void {
    const notification = document.getElementById('notification')
    const notificationHeader = document.getElementById('notification_header')
    const notificationBody = document.getElementById('notification_body')
    const notificationTimeoutId = document.getElementById('notification_timeout_id') as HTMLInputElement | null

    if (!notification || !notificationHeader || !notificationBody || !notificationTimeoutId) {
      return
    }

    clearTimeout(Number(notificationTimeoutId.value))

    notification.classList.remove('error', 'success')
    if (type === 'error') {
      notification.classList.add('error')
    } else if (type === 'success') {
      notification.classList.add('success')
    }

    notificationHeader.textContent = header
    notificationBody.textContent = body

    if (notification.classList.contains('active')) {
      notification.classList.remove('active')
      setTimeout(() => notification.classList.add('active'), 100)
    } else {
      notification.classList.add('active')
    }

    const newTimeoutId = window.setTimeout(() => {
      notification.classList.remove('active')
    }, 7000)
    notificationTimeoutId.value = newTimeoutId.toString()
  }
}
