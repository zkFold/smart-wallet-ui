module ZkFold.Cardano.SmartWallet.Api.Batch (
  batchTxs,
) where

import Cardano.Api qualified as CApi
import Cardano.Api.Internal.Fees qualified as CApi
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as CApi
import Control.Lens ((^.))
import Data.List (elemIndex, union)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Ratio ((%))
import Data.Set qualified as Set
import GHC.IsList (IsList (fromList), toList)
import GeniusYield.Imports hiding (toList)
import GeniusYield.TxBuilder
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
  bwisResolvedWithIns <-
    mapM
      ( \ZKBatchWalletInfo{..} -> do
          (txBodyContent, inUtxos) <- obtainTxBodyContentBuildTx' $ getTxBody zkbwiTx
          walletScript <- initializeWalletScripts zkbwiEmail
          pure ((txBodyContent, walletScript, zkbwiPaymentKeyHash), inUtxos)
      )
      bwis
  let bwisResolved = fmap fst bwisResolvedWithIns
      inUtxos = foldMap snd bwisResolvedWithIns
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
                    { CApi.txMintValue = combineMintValue (CApi.txMintValue accBodyContent) (CApi.txMintValue txBodyContent)
                    , CApi.txCertificates =
                        combineTxCertificates
                          (CApi.txCertificates accBodyContent)
                          (CApi.txCertificates txBodyContent)
                          ( updateCertRedeemer
                              (GYCredentialByScript (scriptHash (zkiwsCheckSig walletScript)) & stakeCredentialToApi)
                              pastOutsNum
                              (findSignatoryIndex oreqSigs pkh)
                          )
                    , CApi.txWithdrawals =
                        combineTxWithdrawals
                          (CApi.txWithdrawals accBodyContent)
                          (CApi.txWithdrawals txBodyContent)
                          ( updateWdrlRedeemer
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
                  stakeCred = GYCredentialByScript $ scriptHash (zkiwsCheckSig fstWalletScript)
                  stakeAddr = stakeAddressFromCredential nid stakeCred
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
                                  ( updateWdrlRedeemer
                                      (stakeAddressToApi stakeAddr)
                                      0
                                      (findSignatoryIndex oreqSigs pkh)
                                  )
                                  ls
                              )
                      , CApi.txCertificates = case CApi.txCertificates fstTxBodyContent of
                          CApi.TxCertificatesNone -> CApi.TxCertificatesNone
                          CApi.TxCertificates sbe ls ->
                            CApi.TxCertificates
                              sbe
                              ( (fmap . fmap)
                                  ( updateCertRedeemer
                                      (stakeCredentialToApi stakeCred)
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
  eh <- eraHistory
  ss <- systemStart
  let combinedTxBodyContentWithColl =
        combinedTxBodyContent
          { CApi.txReturnCollateral = CApi.TxReturnCollateral CApi.BabbageEraOnwardsConway $ txOutToApi $ GYTxOut (utxoAddress (fromJust acollUTxO)) (collBalance `valueMinus` valueFromLovelace balanceNeeded) Nothing Nothing
          , CApi.txTotalCollateral = CApi.TxTotalCollateral CApi.BabbageEraOnwardsConway (Ledger.Coin balanceNeeded)
          }
  let inUtxosWithColl = inUtxos <> collateralUtxos
      inUtxosWithCollApi = utxosToApi inUtxosWithColl
      ppApi = CApi.LedgerProtocolParameters pp
      era = CApi.toCardanoEra CApi.ShelleyBasedEraConway
      ehApi = CApi.toLedgerEpochInfo eh
      ecombinedTxBody = do
        txBodyForExUnits <- CApi.createTransactionBody CApi.ShelleyBasedEraConway combinedTxBodyContentWithColl & first CApi.TxBodyError
        exUnitsMapWithLogs <-
          first CApi.TxBodyErrorValidityInterval $
            CApi.evaluateTransactionExecutionUnits
              era
              ss
              ehApi
              ppApi
              inUtxosWithCollApi
              txBodyForExUnits
        let exUnitsMap = Map.map (fmap snd) exUnitsMapWithLogs
        exUnitsMap' <-
          case Map.mapEither id exUnitsMap of
            (failures, exUnitsMap') ->
              CApi.handleExUnitsErrors
                (CApi.txScriptValidityToScriptValidity (CApi.txScriptValidity combinedTxBodyContentWithColl))
                failures
                exUnitsMap'
        finalTxBodyContent <- CApi.substituteExecutionUnits exUnitsMap' combinedTxBodyContentWithColl
        CApi.createTransactionBody CApi.ShelleyBasedEraConway finalTxBodyContent & first CApi.TxBodyError
  combinedTxBody <- either (throwError . GYBuildTxException . GYBuildTxBodyErrorAutoBalance) pure ecombinedTxBody
  pure $ unsignedTx $ txBodyFromApi combinedTxBody
 where
  updateWitRedeemer outIx sigIx swi sw =
    CApi.ScriptWitness swi $
      case sw of
        CApi.SimpleScriptWitness _ _ -> sw
        CApi.PlutusScriptWitness slie psv psori sd _sr eu ->
          CApi.PlutusScriptWitness slie psv psori sd (Signature outIx sigIx & redeemerFromPlutusData & redeemerToApi) eu
  updateCertRedeemer ::
    CApi.StakeCredential ->
    Integer ->
    Integer ->
    Maybe (CApi.StakeCredential, CApi.Witness CApi.WitCtxStake ApiEra) ->
    Maybe (CApi.StakeCredential, CApi.Witness CApi.WitCtxStake ApiEra)
  updateCertRedeemer reqStakeCred outIx sigIx msw =
    case msw of
      Nothing -> Nothing
      Just (sc, wit) ->
        if sc == reqStakeCred
          then case wit of
            CApi.KeyWitness _ -> Just (sc, wit)
            CApi.ScriptWitness swi sw ->
              Just
                ( sc
                , updateWitRedeemer outIx sigIx swi sw
                )
          else Just (sc, wit)

  updateWdrlRedeemer ::
    CApi.StakeAddress ->
    Integer ->
    Integer ->
    (CApi.StakeAddress, Ledger.Coin, CApi.BuildTxWith CApi.BuildTx (CApi.Witness CApi.WitCtxStake ApiEra)) ->
    (CApi.StakeAddress, Ledger.Coin, CApi.BuildTxWith CApi.BuildTx (CApi.Witness CApi.WitCtxStake ApiEra))
  updateWdrlRedeemer reqStakeAddr outIx sigIx (stakeAddr, coin, CApi.BuildTxWith wit) =
    if stakeAddr == reqStakeAddr
      then case wit of
        CApi.KeyWitness _ -> (stakeAddr, coin, CApi.BuildTxWith wit)
        CApi.ScriptWitness swi sw ->
          ( stakeAddr
          , coin
          , CApi.BuildTxWith $
              updateWitRedeemer outIx sigIx swi sw
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

  combineTxCertificates a b updateF = combineTxCertificates' a $ case b of
    CApi.TxCertificatesNone -> CApi.TxCertificatesNone
    CApi.TxCertificates sbe ls ->
      CApi.TxCertificates sbe $ fmap (fmap updateF) ls

  combineTxCertificates' a CApi.TxCertificatesNone = a
  combineTxCertificates' CApi.TxCertificatesNone b = b
  combineTxCertificates' (CApi.TxCertificates sbe a) (CApi.TxCertificates _sbe b) =
    CApi.TxCertificates sbe $ fromList (toList a <> toList b)

  combineMintValue CApi.TxMintNone b = b
  combineMintValue a CApi.TxMintNone = a
  combineMintValue (CApi.TxMintValue sbe a) (CApi.TxMintValue _sbe b) = CApi.TxMintValue sbe (a <> b)

  headMaybe :: [a] -> Maybe a
  headMaybe [] = Nothing
  headMaybe (x : _) = Just x