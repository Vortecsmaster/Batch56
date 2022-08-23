{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Batch56VestingContract where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)       
import           Data.Map             as Map                  
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

--ON-CHAIN
data VestingDatum = VestingDatum
                    { beneficiary :: PaymentPubKeyHash
                    , releaseTime :: POSIXTime
                    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE vestingValidator #-}
vestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
vestingValidator datum _ sContext = traceIfFalse "Beneficiary's signature missing" signedByBeneficiary &&
                                    traceIfFalse "Deadline not reached" releaseTimeReached
   where
    info :: TxInfo
    info = scriptContextTxInfo sContext

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedby info $ unPaymentPubKeyHash $ beneficiary datum

    releaseTimeReached :: Bool
    releaseTimeReached = contains (from $ releaseTime datum) $ txInfoValidRange info

data Vesting                                                          -- Enconding the type of the datum and the redeemer 
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum                    -- Instance for the Datum
    type instance RedeemerType Vesting = ()                           -- Instance for the Reederm, unit since its not been used

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| vestingValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- OFF-CHAIN 

data GiveParams = GiveParams
     { gpBeneficiary :: !PaymentPubKeyHash
     , gpReleaseTime    :: !POSIXTime 
     , gpAmount       :: !Integer
     } deriving (Generic, ToJSON, FromJSON, ToSchema)


type VestingSchema =
        Endpoint "give" GiveParams
    .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
          let dat = VestingDatum
                    { beneficiary = gpBeneficiary gp
                    , releaseTime    = gpReleaseTime gp
                    }
              tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp 
          ledgerTx <- submitTxConstraints typedValidator tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          logInfo @String $ printf "Made a gift of %d lovelace to %s with release time = %s"
           (gpAmount gp)
           (show $ gpBeneficiary gp)
           (show $ gpReleaseTime gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ "No gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> beneficiary d == pkh && releaseTime d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema
mkKnownCurrencies []
