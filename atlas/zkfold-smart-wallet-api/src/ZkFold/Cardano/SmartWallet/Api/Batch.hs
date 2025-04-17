module ZkFold.Cardano.SmartWallet.Api.Batch (

) where

import Cardano.Api qualified as CApi
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as CApi
import Cardano.Api.Shelley qualified as CApi.S
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Conway.Scripts qualified as Ledger
import Cardano.Ledger.Plutus.Language qualified as Ledger
import Data.Foldable (Foldable (foldl'))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.Types

{- | Combine zk smart-wallet transactions into a single transaction.

Our strategy to make a batch is to combine individually built transactions. It has following benefits:

* Wallets individually pay for their outputs. Otherwise we'll need big overhaul (or rather a custom version) of coin selection algorithm, losing the robustness of existing algorithm.
* We continue exercising robust coin selection algorithm of Atlas.
* Since transactions are already built (and can be built in parallel), combining is now quick.
* Since transactions of users are individually built, they have complete freedom over it, like using different coin selection strategies.

However, currently it is not clear if fees of combined transaction will be higher or lower than sum of fees of individual transactions due to the change in execution units of involved scripts as script context is larger now. If it is higher, this strategy would not work.

__NOTE__: We assume transactions provided are those returned by our API when sending funds from our zk based smart-wallet. This combining function need not necessarily work for other transactions.
-}
combineTxs :: (GYTxSpecialQueryMonad m) => [GYTx] -> m GYTx
combineTxs txs = do
  {- Following is the high level strategy:
    * We obtain `TxBodyContent` with `BuildTx` parameter.
  -}
  -- We ignore for key witnesses since they won't be valid anyway.
  let txBodies = map getTxBody txs
  txBodiesContent <- mapM (obtainTxBodyContentBuildTx) txBodies
  undefined

obtainTxBodyContentBuildTx :: (GYTxSpecialQueryMonad m) => GYTxBody -> m (CApi.TxBodyContent CApi.BuildTx ApiEra)
obtainTxBodyContentBuildTx (txBodyToApi -> txBody@(CApi.ShelleyTxBody sbe ltxBody lscripts scriptData _ _)) = do
  let
    -- We obtained `TxBodyContent ViewTx`. Now we need to obtain `BuildTx` version of it.
    txBodyContentViewTx = CApi.getTxBodyContent txBody

  resolvedSpendIns <- utxosAtTxOutRefsWithDatums $ map (txOutRefFromApi . fst) (CApi.txIns txBodyContentViewTx)
  resolvedRefIns <- utxosAtTxOutRefs $ map txOutRefFromApi $ case CApi.txInsReference txBodyContentViewTx of
    CApi.TxInsReferenceNone -> []
    CApi.TxInsReference _ refIns -> refIns
  let refScripts :: Map GYScriptHash (GYTxOutRef, GYAnyScript) =
        foldl'
          ( \acc GYUTxO{..} -> case utxoRefScript of
              Nothing -> acc
              Just as -> Map.insert (hashAnyScript as) (utxoRef, as) acc
          )
          mempty
          (utxosToList resolvedRefIns <> map fst resolvedSpendIns)
  pp <- protocolParams
  pure $
    CApi.TxBodyContent
      { txWithdrawals = case CApi.txWithdrawals txBodyContentViewTx of
          CApi.TxWithdrawalsNone -> CApi.TxWithdrawalsNone
          CApi.TxWithdrawals wsbe wdrls -> CApi.TxWithdrawals wsbe $ map (wdrlFromApi refScripts) wdrls
      , txVotingProcedures = Nothing
      , txValidityUpperBound = CApi.txValidityUpperBound txBodyContentViewTx
      , txValidityLowerBound = CApi.txValidityLowerBound txBodyContentViewTx
      , txUpdateProposal = CApi.txUpdateProposal txBodyContentViewTx
      , txTreasuryDonation = CApi.txTreasuryDonation txBodyContentViewTx
      , txTotalCollateral = CApi.txTotalCollateral txBodyContentViewTx
      , txScriptValidity = CApi.txScriptValidity txBodyContentViewTx
      , txReturnCollateral = CApi.txReturnCollateral txBodyContentViewTx
      , txProtocolParams = CApi.BuildTxWith $ Just $ CApi.S.LedgerProtocolParameters pp
      , txProposalProcedures = Nothing
      , txOuts = CApi.txOuts txBodyContentViewTx
      , txMintValue = CApi.TxMintNone
      , txMetadata = CApi.txMetadata txBodyContentViewTx
      , txInsReference = CApi.txInsReference txBodyContentViewTx
      , txInsCollateral = CApi.txInsCollateral txBodyContentViewTx
      , txIns = map (inFromApi refScripts) resolvedSpendIns
      , txFee = CApi.txFee txBodyContentViewTx
      , txExtraKeyWits = CApi.txExtraKeyWits txBodyContentViewTx
      , txCurrentTreasuryValue = CApi.txCurrentTreasuryValue txBodyContentViewTx
      , txCertificates = CApi.TxCertificatesNone
      , txAuxScripts = CApi.txAuxScripts txBodyContentViewTx
      }
 where
  findScript sh =
    find
      ( \case
          Ledger.TimelockScript ts -> CApi.fromAllegraTimelock ts & simpleScriptFromApi & hashSimpleScript & \sh' -> sh' == sh
          Ledger.PlutusScript ps ->
            Ledger.withPlutusScript
              ps
              ( \ps' ->
                  scriptHashFromLedger (Ledger.hashPlutusScript ps') == sh
              )
      )
      lscripts
  inFromApi refScripts (utxo, mdatum) =
    ( utxoRef utxo & txOutRefToApi
    , case fromJust $ addressToPaymentCredential $ utxoAddress utxo of
        GYCredentialByKey _ -> CApi.BuildTxWith $ CApi.KeyWitness CApi.KeyWitnessForSpending
        GYCredentialByScript sh ->
          case Map.lookup sh refScripts of
            Nothing ->
              case findScript sh of
                Nothing -> CApi.BuildTxWith $ CApi.KeyWitness CApi.KeyWitnessForSpending -- TODO: To throw an app error here?
                Just (Ledger.TimelockScript ts) ->
                  CApi.BuildTxWith $ CApi.ScriptWitness CApi.ScriptWitnessForSpending $ CApi.SimpleScriptWitness CApi.SimpleScriptInConway $ CApi.SScript (CApi.fromAllegraTimelock ts)
                Just (Ledger.PlutusScript ps) ->
                  CApi.BuildTxWith $
                    CApi.ScriptWitness CApi.ScriptWitnessForSpending $
                      ( case ps of
                          Ledger.ConwayPlutusV1 ps' -> Ledger.plutusBinary ps' & Ledger.unPlutusBinary & scriptFromSerialisedScript @'PlutusV1 & validatorToApiPlutusScriptWitness
                          Ledger.ConwayPlutusV2 ps' -> Ledger.plutusBinary ps' & Ledger.unPlutusBinary & scriptFromSerialisedScript @'PlutusV2 & validatorToApiPlutusScriptWitness
                          Ledger.ConwayPlutusV3 ps' -> Ledger.plutusBinary ps' & Ledger.unPlutusBinary & scriptFromSerialisedScript @'PlutusV3 & validatorToApiPlutusScriptWitness
                      )
                        ( case utxoOutDatum utxo of
                            GYOutDatumInline _ -> CApi.InlineScriptDatum
                            GYOutDatumNone -> CApi.ScriptDatumForTxIn Nothing
                            GYOutDatumHash _ -> CApi.ScriptDatumForTxIn $ datumToApi' <$> mdatum
                        )
                        (redeemerToApi unitRedeemer)
                        (CApi.ExecutionUnits 0 0)
            Just (ref, as) -> CApi.BuildTxWith $
              CApi.ScriptWitness CApi.ScriptWitnessForSpending $
                case as of
                  GYPlutusScript ps ->
                    referenceScriptToApiPlutusScriptWitness
                      ref
                      ps
                      ( case utxoOutDatum utxo of
                          GYOutDatumInline _ -> CApi.InlineScriptDatum
                          GYOutDatumNone -> CApi.ScriptDatumForTxIn Nothing
                          GYOutDatumHash _ -> CApi.ScriptDatumForTxIn $ datumToApi' <$> mdatum
                      )
                      (redeemerToApi unitRedeemer)
                      (CApi.ExecutionUnits 0 0)
                  GYSimpleScript _ss -> CApi.SimpleScriptWitness CApi.SimpleScriptInConway $ CApi.SReferenceScript $ txOutRefToApi ref
    )
  wdrlFromApi refScripts (stakeAddr, coin, _) =
    ( stakeAddr
    , coin
    , case stakeAddressToCredential (stakeAddressFromApi stakeAddr) of
        GYCredentialByKey _ -> CApi.BuildTxWith $ CApi.KeyWitness CApi.KeyWitnessForStakeAddr
        GYCredentialByScript sh ->
          case Map.lookup sh refScripts of
            Nothing ->
              case findScript sh of
                Nothing -> CApi.BuildTxWith $ CApi.KeyWitness CApi.KeyWitnessForStakeAddr -- TODO: To throw an app error here?
                Just (Ledger.TimelockScript ts) ->
                  CApi.BuildTxWith $ CApi.ScriptWitness CApi.ScriptWitnessForStakeAddr $ CApi.SimpleScriptWitness CApi.SimpleScriptInConway $ CApi.SScript (CApi.fromAllegraTimelock ts)
                Just (Ledger.PlutusScript ps) ->
                  CApi.BuildTxWith $
                    CApi.ScriptWitness CApi.ScriptWitnessForStakeAddr $
                      ( case ps of
                          Ledger.ConwayPlutusV1 ps' -> Ledger.plutusBinary ps' & Ledger.unPlutusBinary & scriptFromSerialisedScript @'PlutusV1 & stakeValidatorToApiPlutusScriptWitness
                          Ledger.ConwayPlutusV2 ps' -> Ledger.plutusBinary ps' & Ledger.unPlutusBinary & scriptFromSerialisedScript @'PlutusV2 & stakeValidatorToApiPlutusScriptWitness
                          Ledger.ConwayPlutusV3 ps' -> Ledger.plutusBinary ps' & Ledger.unPlutusBinary & scriptFromSerialisedScript @'PlutusV3 & stakeValidatorToApiPlutusScriptWitness
                      )
                        (redeemerToApi unitRedeemer)
                        (CApi.ExecutionUnits 0 0)
            Just (ref, as) -> CApi.BuildTxWith $
              CApi.ScriptWitness CApi.ScriptWitnessForStakeAddr $
                case as of
                  GYPlutusScript ps ->
                    referenceScriptToApiPlutusScriptWitness
                      ref
                      ps
                      CApi.NoScriptDatumForStake
                      (redeemerToApi unitRedeemer)
                      (CApi.ExecutionUnits 0 0)
                  GYSimpleScript _ss -> CApi.SimpleScriptWitness CApi.SimpleScriptInConway $ CApi.SReferenceScript $ txOutRefToApi ref
    )
