{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}

module Batch56NFTBounty where

import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude hiding (pure, (<$>), Semigroup (..))
import PlutusTx.IsData.Class (toBuiltinData)

import           Ledger                    
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Ledger.Value           as Value
import           Ledger.TimeSlot

import           Plutus.Contract

import qualified Plutus.Trace as Trace
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import qualified Control.Monad.Freer.Extras as Extras

import           Control.Monad             (void)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Default               (Default (..))
import           Data.Text                  (Text)
import           Data.Void
import           Data.Map as Map
import qualified Prelude
import           Prelude (IO, Show, String, show, Semigroup (..))
import           Text.Printf          (printf)
import           GHC.Generics           (Generic)

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))

--import qualified Plutus.Script.Utils.V2.Scripts  as UtilsV2
--import qualified Plutus.V2.Ledger.Api

--ON-CHAIN
data MathBountyDatum = MBD
    { mBounty :: Integer }


{-# INLINABLE mathBountyValidator  #-}
mathBountyValidator :: MathBountyDatum -> Integer -> ScriptContext -> Bool
mathBountyValidator datum redeemer sContext =  traceIfFalse "Wrong guess!" ((mBounty datum) == redeemer*redeemer) 

data MathBounty
instance Scripts.ValidatorTypes MathBounty where
    type instance RedeemerType MathBounty = Integer
    type instance DatumType MathBounty = MathBountyDatum

bountyValidator :: Scripts.TypedValidator MathBounty
bountyValidator = Scripts.mkTypedValidator @MathBounty
    $$(PlutusTx.compile [|| mathBountyValidator ||])
    $$(PlutusTx.compile [|| wrapping ||])
     where
       wrapping  = Scripts.wrapValidator @MathBountyDatum @Integer   -- Change after Vasil to unTypedValidator auxiliary function

validator :: Validator
validator = Scripts.validatorScript bountyValidator

bountyAddress :: Ledger.Address       
bountyAddress = scriptAddress validator


{-# INLINABLE nftMintingPolicy  #-}
nftMintingPolicy :: TxOutRef -> TokenName ->  () -> ScriptContext -> Bool
nftMintingPolicy oref tname _ sContext = traceIfFalse "UTxO not consumed" hasUTxO               &&
                                         traceIfFalse "Wrong ammount minted" checkMintedAmount 
    where
      info :: TxInfo
      info = scriptContextTxInfo sContext

      hasUTxO :: Bool
      hasUTxO = any (\utxo -> txInInfoOutRef utxo == oref) $ txInfoInputs info

      checkMintedAmount :: Bool
      checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tname', amount)]   ->  tname' == tname && amount == 1
        _                       ->  False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tname = mkMintingPolicyScript $
             $$(PlutusTx.compile [|| \oref' tname' -> Scripts.wrapMintingPolicy $ nftMintingPolicy oref' tname' ||])
             `PlutusTx.applyCode`
             PlutusTx.liftCode oref
             `PlutusTx.applyCode`
             PlutusTx.liftCode tname

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tname = scriptCurrencySymbol $ policy oref tname

--OFF-CHAIN

data BountyParams = BP 
                  { bMathBounty :: Integer
                  , bAmount     :: Integer } deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

type MathBountySchema = 
        Endpoint "bounty" BountyParams
    .\/ Endpoint "solution" Integer

endpoints :: Contract () MathBountySchema Text ()
endpoints = awaitPromise (bounty' `select` solution') >> endpoints
  where
    bounty' =  endpoint @"bounty" bounty
    solution' = endpoint @"solution" solution

mkSchemaDefinitions ''MathBountySchema
mkKnownCurrencies []


bounty :: BountyParams -> Contract () MathBountySchema Text ()
bounty (BP bounty amount) = do
    let datum = MBD bounty 
        tx = Constraints.mustPayToTheScript datum (Ada.lovelaceValueOf amount)
    ledgerTx <- submitTxConstraints bountyValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Some bounty created of amount =  %s" (show amount)

solution ::  Integer -> Contract () MathBountySchema Text ()
solution guess = do
                 utxos <- utxosAt bountyAddress
                 logInfo @String $ printf "The utxos: %s " (show $ Map.toList utxos )
                 case Map.toList utxos of
                  []             -> logInfo @String $ printf "No UTxOs on the Contract!"
                  (oref,a):utxos -> do
                                    let tname = TokenName "Batch__BountyWinner"
                                        val = Value.singleton (curSymbol oref tname) tname 1 
                                        lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
                                                  Constraints.otherScript validator <>
                                                  Constraints.mintingPolicy (policy oref tname)
                                        tx = Constraints.mustSpendScriptOutput oref (Redeemer $ toBuiltinData guess) <>
                                             Constraints.mustMintValue val 
                                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @String $ printf "Proposed solution is: %s" (show guess)       

--SIMULATION

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    callEndpoint @"bounty" h1 $ BP
                    { bMathBounty = 100
                  , bAmount     = 51000000
                    }
    void $ Emulator.waitNSlots 5
    callEndpoint @"solution" h2 $ 5
    void $ Emulator.waitNSlots 2
    callEndpoint @"solution" h3 $ 10
    void $ Emulator.waitNSlots 11  
