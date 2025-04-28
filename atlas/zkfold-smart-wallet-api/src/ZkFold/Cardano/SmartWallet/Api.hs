module ZkFold.Cardano.SmartWallet.Api (
  addressFromEmail,
  createWallet,
  createWallet',
  sendFundsWithCreation,
  sendFundsWithCreation',
  findMintedAuthTokens,
  sendFunds,
  sendFunds',
  batchTxs,
) where

import ZkFold.Cardano.SmartWallet.Api.Batch
import ZkFold.Cardano.SmartWallet.Api.Create
import ZkFold.Cardano.SmartWallet.Api.Send
