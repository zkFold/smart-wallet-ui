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
import Data.Foldable (Foldable (foldl'))
import Data.List (elemIndex, union)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import GeniusYield.Imports
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder.Class
import GeniusYield.Types
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
combineTxs :: (GYTxSpecialQueryMonad m) => NE.NonEmpty GYTx -> m GYTx
combineTxs txs = do
  {- Following is the high level strategy:
    * We obtain `TxBodyContent` with `BuildTx` parameter.
  -}
  -- We ignore for key witnesses since they won't be valid anyway.
  let txBodies = fmap getTxBody txs
  txBodiesContent <- mapM obtainTxBodyContentBuildTx txBodies
  pp <- protocolParams
  -- Ordered list of required extra-key witnesses.
  let oreqSigs =
        foldMap
          ( CApi.txExtraKeyWits >>> \case
              CApi.TxExtraKeyWitnessesNone -> mempty
              CApi.TxExtraKeyWitnesses _ ks -> Set.fromList ks
          )
          txBodiesContent
          & Set.toList
      combinedTxBodyContent =
        foldl'
          ( \(accBodyContent, pastOutsNum) txBodyContent ->
              ( accBodyContent
                  { CApi.txWithdrawals = combineTxWithdrawals (CApi.txWithdrawals accBodyContent) (CApi.txWithdrawals txBodyContent) (updateRedeemer pastOutsNum (findSignatoryIndex oreqSigs (CApi.txExtraKeyWits txBodyContent)))
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
          ( let fstTxBodyContent = NE.head txBodiesContent
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
                                    0
                                    (findSignatoryIndex oreqSigs $ CApi.txExtraKeyWits fstTxBodyContent)
                                )
                                ls
                            )
                    }
                , fromIntegral (length (CApi.txOuts fstTxBodyContent))
                )
          )
          (NE.tail txBodiesContent)
  undefined
 where
  updateRedeemer ::
    Integer ->
    Integer ->
    (CApi.StakeAddress, Ledger.Coin, CApi.BuildTxWith CApi.BuildTx (CApi.Witness CApi.WitCtxStake ApiEra)) ->
    (CApi.StakeAddress, Ledger.Coin, CApi.BuildTxWith CApi.BuildTx (CApi.Witness CApi.WitCtxStake ApiEra))
  updateRedeemer outIx sigIx (stakeAddr, coin, CApi.BuildTxWith wit) = case wit of
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
  -- TODO: Use apperrors here.
  findSignatoryIndex oreqSigs CApi.TxExtraKeyWitnessesNone = error "TODO:"
  findSignatoryIndex oreqSigs (CApi.TxExtraKeyWitnesses _ ks) = elemIndex (head ks) oreqSigs & fromJust & fromIntegral

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

mergeTxBodyContent (_oreqSigs, _pastOutsNum) [] = error "ZkFold.Cardano.SmartWallet.Api.Batch.mergeTxBodyContent: absurd"
mergeTxBodyContent (_oreqSigs, _pastOutsNum) [txBodyContent] = txBodyContent
mergeTxBodyContent (oreqSigs, pastOutsNum) (txBodyContent1 : txBodyContent2 : txBodyContents) =
  mergeTxBodyContent
    (oreqSigs, pastOutsNum + (length $ CApi.txOuts txBodyContent1))
    ( CApi.TxBodyContent
        { txWithdrawals =
            -- We need to update redeemer of both tx to have correct index of signatory. And we need to update redeemer of second tx to have correct index for it's output.
            let
             in -- tx1WdrlRedeemer = Signature (pastOutsNum) (findSignatoryIndex oreqSigs )
                undefined
        , txVotingProcedures = Nothing
        , txValidityUpperBound = combineValidityUpperBound (CApi.txValidityUpperBound txBodyContent1) (CApi.txValidityUpperBound txBodyContent2)
        , txValidityLowerBound = combineValidityLowerBound (CApi.txValidityLowerBound txBodyContent1) (CApi.txValidityLowerBound txBodyContent2)
        , txUpdateProposal = CApi.TxUpdateProposalNone
        , txTreasuryDonation = Nothing
        , txTotalCollateral = CApi.TxTotalCollateralNone -- Updated later.
        , txScriptValidity = CApi.txScriptValidity txBodyContent1
        , txReturnCollateral = CApi.TxReturnCollateralNone -- Updated later.
        , txProtocolParams = CApi.txProtocolParams txBodyContent1
        , txProposalProcedures = Nothing
        , txOuts = CApi.txOuts txBodyContent1 <> CApi.txOuts txBodyContent2
        , txMintValue = CApi.TxMintNone
        , txMetadata = CApi.TxMetadataNone
        , txInsReference = combineTxInsReference (CApi.txInsReference txBodyContent1) (CApi.txInsReference txBodyContent2)
        , txInsCollateral = combineTxInsCollateral (CApi.txInsCollateral txBodyContent1) (CApi.txInsCollateral txBodyContent2)
        , txIns = CApi.txIns txBodyContent1 `union` CApi.txIns txBodyContent2
        , txFee = CApi.txFee txBodyContent1 `addTxFee` CApi.txFee txBodyContent2
        , txExtraKeyWits = CApi.TxExtraKeyWitnesses CApi.AlonzoEraOnwardsConway oreqSigs
        , txCurrentTreasuryValue = Nothing
        , txCertificates = CApi.TxCertificatesNone
        , txAuxScripts = CApi.TxAuxScriptsNone
        }
        : txBodyContents
    )
 where
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
