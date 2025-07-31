import express from 'express'
import cors from 'cors'
import * as dotenv from 'dotenv'
import { Backend } from 'zkfold-smart-wallet-api'
import * as CSL from '@emurgo/cardano-serialization-lib-browser'
import * as url from 'url'

dotenv.config()

const app = express()
const PORT = 3001

app.use(cors())
app.use(express.json())

// Transaction status endpoint
app.get('/tx_status', async (req, res) => {
  try {
    const q = url.parse(req.url, true).query
    
    if (!q.txId || !q.recipient || typeof q.txId !== 'string' || typeof q.recipient !== 'string') {
      return res.json({ outcome: "failure", reason: "Missing txId or recipient" })
    }

    const txId = q.txId
    const recipient = CSL.Address.from_bech32(q.recipient)
    const backendUrl = process.env.BACKEND_URL || process.env.VITE_BACKEND_URL
    const backendApiKey = process.env.BACKEND_API_KEY || process.env.VITE_BACKEND_API_KEY

    if (!backendUrl) {
      return res.json({ outcome: "failure", reason: "Backend URL not configured" })
    }

    const backend = backendApiKey
      ? new Backend(backendUrl, backendApiKey)
      : new Backend(backendUrl)

    const utxos = await backend.addressUtxo(recipient)
    
    for (const utxo of utxos) {
      if ((utxo as any).ref.transaction_id === txId) {
        return res.json({ outcome: "success", data: utxo })
      }
    }
    
    res.json({ outcome: "pending" })
  } catch (error) {
    console.error('Transaction status check failed:', error)
    res.json({ outcome: "failure", reason: String(error) })
  }
})

app.listen(PORT, () => {
  console.log(`API server running on port ${PORT}`)
})