module ZkFold.Cardano.SmartWallet.Api.Batch (

) where

import Cardano.Api qualified as CApi
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as CApi
import Cardano.Api.Shelley qualified as CApi.S
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Conway.Scripts qualified as Ledger
import Cardano.Ledger.Plutus.Language qualified as Ledger
import Control.Lens ((^.))
import Data.Foldable (Foldable (foldl'))
import Data.List (elemIndex, union)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Ratio ((%))
import Data.Set qualified as Set
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
import ZkFold.Cardano.SmartWallet.Api.Create (initializeWalletScripts)
import ZkFold.Cardano.SmartWallet.Types
import ZkFold.Cardano.UPLC.Wallet.Types (Signature (..))

{- | Combine zk smart-wallet transactions into a single transaction.

Our strategy to make a batch is to combine individually built transactions. It has following benefits:

* Wallets individually pay for their outputs. Otherwise we'll need big overhaul (or rather a custom version) of coin selection algorithm, losing the robustness of existing algorithm.
* We continue exercising robust coin selection algorithm of Atlas.
* Since transactions are already built (and can be built in parallel), combining is now quick.
* Since transactions of users are individually built, they have complete freedom over it, like using different coin selection strategies.

However, currently it is not clear if fees of combined transaction will be higher or lower than sum of fees of individual transactions due to the change in execution units of involved scripts as script context is larger now. If it is higher, this strategy would not work.

__NOTE__: We assume transactions provided are those returned by our API when sending funds from our zk based smart-wallet. This combining function need not necessarily work for other transactions.
-}
batchTxs :: (ZKWalletQueryMonad m) => NE.NonEmpty ZKBatchWalletInfo -> m GYTx
batchTxs bwis = do
  {- Following is the high level strategy:
    * We obtain `TxBodyContent` with `BuildTx` parameter.
    * We concatenate the `TxBodyContent`s by adding up the fees and updating redeemers for withdrawals script.
  -}
  -- We ignore for key witnesses since they won't be valid anyway.
  bwisResolved <-
    mapM
      ( \ZKBatchWalletInfo{..} -> do
          txBodyContent <- obtainTxBodyContentBuildTx $ getTxBody $ zkbwiTx
          walletScript <- initializeWalletScripts zkbwiEmail
          pure (txBodyContent, walletScript, zkbwiPaymentKeyHash)
      )
      bwis
  pp <- protocolParams
  nid <- networkId
  -- Ordered list of required extra-key witnesses.
  let oreqSigs =
        foldMap
          ( (\(a, _b, _c) -> a) >>> CApi.txExtraKeyWits >>> \case
              CApi.TxExtraKeyWitnessesNone -> mempty
              CApi.TxExtraKeyWitnesses _ ks -> Set.fromList ks
          )
          bwisResolved
          & Set.union (Set.fromList $ NE.toList $ fmap (zkbwiPaymentKeyHash >>> paymentKeyHashToApi) bwis)
          & Set.toList
      combinedTxBodyContent =
        fst $
          foldl'
            ( \(accBodyContent, pastOutsNum) (txBodyContent, walletScript, paymentKeyHashToApi -> pkh) ->
                ( accBodyContent
                    { CApi.txWithdrawals =
                        combineTxWithdrawals
                          (CApi.txWithdrawals accBodyContent)
                          (CApi.txWithdrawals txBodyContent)
                          ( updateRedeemer
                              (stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash (zkiwsCheckSig walletScript)) & stakeAddressToApi)
                              pastOutsNum
                              (findSignatoryIndex oreqSigs pkh)
                          )
                    , CApi.txValidityUpperBound = combineValidityUpperBound (CApi.txValidityUpperBound accBodyContent) (CApi.txValidityUpperBound txBodyContent)
                    , CApi.txValidityLowerBound = combineValidityLowerBound (CApi.txValidityLowerBound accBodyContent) (CApi.txValidityLowerBound txBodyContent)
                    , CApi.txOuts = CApi.txOuts accBodyContent <> CApi.txOuts txBodyContent
                    , CApi.txInsReference = combineTxInsReference (CApi.txInsReference accBodyContent) (CApi.txInsReference txBodyContent)
                    , CApi.txInsCollateral = combineTxInsCollateral (CApi.txInsCollateral accBodyContent) (CApi.txInsCollateral txBodyContent)
                    , CApi.txIns = CApi.txIns accBodyContent `union` CApi.txIns txBodyContent
                    , CApi.txFee = CApi.txFee accBodyContent `addTxFee` CApi.txFee txBodyContent
                    }
                , pastOutsNum + fromIntegral (length (CApi.txOuts txBodyContent))
                )
            )
            ( let (fstTxBodyContent, fstWalletScript, paymentKeyHashToApi -> pkh) = NE.head bwisResolved
                  stakeAddr = stakeAddressFromCredential nid (GYCredentialByScript $ scriptHash (zkiwsCheckSig fstWalletScript))
               in ( fstTxBodyContent
                      { CApi.txExtraKeyWits = CApi.TxExtraKeyWitnesses CApi.AlonzoEraOnwardsConway oreqSigs
                      , CApi.txReturnCollateral = CApi.TxReturnCollateralNone -- Updated later.
                      , CApi.txTotalCollateral = CApi.TxTotalCollateralNone -- Updated later.
                      , CApi.txWithdrawals = case CApi.txWithdrawals fstTxBodyContent of
                          CApi.TxWithdrawalsNone -> CApi.TxWithdrawalsNone
                          CApi.TxWithdrawals sbe ls ->
                            CApi.TxWithdrawals
                              sbe
                              ( map
                                  ( updateRedeemer
                                      (stakeAddressToApi stakeAddr)
                                      0
                                      (findSignatoryIndex oreqSigs pkh)
                                  )
                                  ls
                              )
                      }
                  , fromIntegral (length (CApi.txOuts fstTxBodyContent))
                  )
            )
            (NE.tail bwisResolved)
      collateralIns = case CApi.txInsCollateral combinedTxBodyContent of
        CApi.TxInsCollateralNone -> []
        CApi.TxInsCollateral _ ins -> fmap txOutRefFromApi ins
  collateralUtxos <- utxosAtTxOutRefs collateralIns
  let collBalance = foldMapUTxOs utxoValue collateralUtxos
      collLovelace = valueSplitAda collBalance & fst
      Ledger.Coin txFee = case CApi.txFee combinedTxBodyContent of
        CApi.TxFeeExplicit _ fee -> fee
      balanceNeeded :: Integer = ceiling $ (txFee * toInteger (pp ^. ppCollateralPercentageL)) % 100
  when (collLovelace < balanceNeeded) $ do
    throwError $ GYBuildTxException $ GYBuildTxCollateralShortFall (fromIntegral balanceNeeded) (fromIntegral collLovelace)
  let acollUTxO = utxosToList collateralUtxos & headMaybe
  when (isNothing acollUTxO) $ do
    throwError $ GYBuildTxException GYBuildTxNoSuitableCollateral
  let combinedTxBodyContent' =
        combinedTxBodyContent
          { CApi.txReturnCollateral = CApi.TxReturnCollateral CApi.BabbageEraOnwardsConway $ txOutToApi $ GYTxOut (utxoAddress (fromJust acollUTxO)) (collBalance `valueMinus` valueFromLovelace balanceNeeded) Nothing Nothing
          , CApi.txTotalCollateral = CApi.TxTotalCollateral CApi.BabbageEraOnwardsConway (Ledger.Coin balanceNeeded)
          }
  combinedTxBody <- either (throwError . GYBuildTxException . GYBuildTxBodyErrorAutoBalance . CApi.TxBodyError) pure $ CApi.createTransactionBody CApi.ShelleyBasedEraConway combinedTxBodyContent'
  undefined
 where
  updateRedeemer ::
    CApi.StakeAddress ->
    Integer ->
    Integer ->
    (CApi.StakeAddress, Ledger.Coin, CApi.BuildTxWith CApi.BuildTx (CApi.Witness CApi.WitCtxStake ApiEra)) ->
    (CApi.StakeAddress, Ledger.Coin, CApi.BuildTxWith CApi.BuildTx (CApi.Witness CApi.WitCtxStake ApiEra))
  updateRedeemer reqStakeAddr outIx sigIx (stakeAddr, coin, CApi.BuildTxWith wit) =
    if stakeAddr == reqStakeAddr
      then case wit of
        CApi.KeyWitness _ -> (stakeAddr, coin, CApi.BuildTxWith wit)
        CApi.ScriptWitness swi sw ->
          ( stakeAddr
          , coin
          , CApi.BuildTxWith $
              CApi.ScriptWitness swi $
                case sw of
                  CApi.SimpleScriptWitness _ _ -> sw
                  CApi.PlutusScriptWitness slie psv psori sd _sr eu ->
                    CApi.PlutusScriptWitness slie psv psori sd (Signature outIx sigIx & redeemerFromPlutusData & redeemerToApi) eu
          )
      else (stakeAddr, coin, CApi.BuildTxWith wit)

  findSignatoryIndex oreqSigs pkh =
    elemIndex pkh oreqSigs
      -- Impossible to fail since we already included this pkh in @oreqSigs@.
      & fromJust
      & fromIntegral

  addTxFee (CApi.TxFeeExplicit sbe a) (CApi.TxFeeExplicit _sbe b) = CApi.TxFeeExplicit sbe (a + b)

  combineValidityLowerBound CApi.TxValidityNoLowerBound b = b
  combineValidityLowerBound a CApi.TxValidityNoLowerBound = a
  combineValidityLowerBound (CApi.TxValidityLowerBound aeo a) (CApi.TxValidityLowerBound _aeo b) = CApi.TxValidityLowerBound aeo (min a b)

  combineValidityUpperBound (CApi.TxValidityUpperBound _sbe Nothing) b = b
  combineValidityUpperBound a (CApi.TxValidityUpperBound _sbe Nothing) = a
  combineValidityUpperBound (CApi.TxValidityUpperBound sbe (Just a)) (CApi.TxValidityUpperBound _sbe (Just b)) = CApi.TxValidityUpperBound sbe (Just (max a b))

  combineTxInsCollateral CApi.TxInsCollateralNone b = b
  combineTxInsCollateral a CApi.TxInsCollateralNone = a
  combineTxInsCollateral (CApi.TxInsCollateral aeo a) (CApi.TxInsCollateral _aeo b) = CApi.TxInsCollateral aeo (a `union` b)

  combineTxInsReference CApi.TxInsReferenceNone b = b
  combineTxInsReference a CApi.TxInsReferenceNone = a
  combineTxInsReference (CApi.TxInsReference beo a) (CApi.TxInsReference _beo b) = CApi.TxInsReference beo (a `union` b)

  combineTxWithdrawals a b updateF = combineTxWithdrawals' a $ case b of
    CApi.TxWithdrawalsNone -> CApi.TxWithdrawalsNone
    CApi.TxWithdrawals sbe ls ->
      CApi.TxWithdrawals
        sbe
        ( map
            updateF
            ls
        )
  combineTxWithdrawals' a CApi.TxWithdrawalsNone = a
  combineTxWithdrawals' CApi.TxWithdrawalsNone b = b
  combineTxWithdrawals' (CApi.TxWithdrawals sbe a) (CApi.TxWithdrawals _sbe b) =
    CApi.TxWithdrawals
      sbe
      (a <> b)

  headMaybe :: [a] -> Maybe a
  headMaybe [] = Nothing
  headMaybe (x : _) = Just x
