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

module Batch56FreeMinting where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
 
--ON-CHAIN 
{-# INLINABLE freeMintingPolicy #-}
freeMintingPolicy :: () -> ScriptContext -> Bool
freeMintingPolicy _ _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy freeMintingPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

--OFF-CHAIN CODE

data MintParams = MintParams
                { mpTokenName :: !TokenName
                , mpAmount    :: !Integer   
                } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mparams = do
               let val = Value.singleton curSymbol (mpTokenName mparams) (mpAmount mparams)
                   lookups = Constraints.mintingPolicy policy
                   tx      = Constraints.mustMintValue val
               ledgerTx <- submitTxConstraintsWith @Void lookups tx
               void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
               Contract.logInfo @String $ printf "Forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []

-- EMULATOR SIMULATION

test :: IO ()
test = runEmulatorTraceIO $ do
       h1 <- activateContractWallet (knownWallet 1) endpoints
       h2 <- activateContractWallet (knownWallet 2) endpoints
       callEndpoint @"mint" h1 $ MintParams 
                               { mpTokenName = "Batch56coin"
                               , mpAmount    = 11000
                               }
       void $ Emulator.waitNSlots 10
       callEndpoint @"mint" h2 $ MintParams 
                               { mpTokenName = "Batch56coin"
                               , mpAmount    = 22000
                               }
       void $ Emulator.waitNSlots 10