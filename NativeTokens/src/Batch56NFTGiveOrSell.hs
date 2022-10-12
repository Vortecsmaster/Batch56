{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NumericUnderscores     #-}

module Batch56NFTGiveOrSell where

import           Control.Monad                  hiding (fmap)
import           Data.Aeson                     (ToJSON, FromJSON)
import           Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Data.Maybe
import           GHC.Generics                   (Generic)
import           Plutus.Contract                as Contract
import           Plutus.Trace.Emulator          as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins              as Builtins
import           Ledger                         hiding (mint, singleton)
import           Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import           Ledger.Ada                     as Ada
import           Ledger.Value                   as Value
import           Prelude                        (IO, Show (..), String, Semigroup(..))
import           Text.Printf                    (printf)
import           Wallet.Emulator.Wallet
import           Control.Monad.Freer.Extras     as Extras
import           CustomRedeemer

--ON-CHAIN

{-# INLINABLE nftMintingPolicy  #-}
nftMintingPolicy :: RedeemerParam -> ScriptContext -> Bool
nftMintingPolicy (RP oref tname gOs) sContext = if gOs then traceIfFalse "Can not sell the NFT"  sellingPath
                                                       else traceIfFalse "Can not give the NFT"  givingPath 
  where
      givingPath :: Bool
      givingPath = traceIfFalse "UTxO not consumed" hasUTxO               &&
                   traceIfFalse "Wrong ammount minted" checkMintedAmount  
      sellingPath :: Bool
      sellingPath = traceIfFalse "UTxO not consumed" hasUTxO               &&
                    traceIfFalse "Wrong ammount minted" checkMintedAmount  &&
                    traceIfFalse "No enough payment" checkPaymentAmount   
               -- &&                traceIfFalse "Seller doesn't get the payment" checkSellerPayment  
      info :: TxInfo
      info = scriptContextTxInfo sContext

      hasUTxO :: Bool
      hasUTxO = any (\utxo -> txInInfoOutRef utxo == oref) $ txInfoInputs info

      checkMintedAmount :: Bool
      checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tname', amount)]   ->  tname' == tname && amount == 1
        _                       ->  False
      -- must enforce uniqueness thrue tokenName with UTxO TxId in oRef 
      checkPaymentAmount :: Bool
      checkPaymentAmount = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy nftMintingPolicy ||])


curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy 


--OFF-CHAIN
type NFTSchema = Endpoint "giveMint" NFTParams

data NFTParams = NFTParams { npTokenName :: !TokenName
                           , npAddress   :: !Address
                           , npReciever  :: !Address
                           } deriving (Generic, FromJSON, ToJSON, Show) 

giveMint :: NFTParams -> Contract w NFTSchema Text ()
giveMint nparams = do
               utxos <- utxosAt $ npAddress nparams
               case Map.keys utxos of
                 []         -> Contract.logError @String "No UTxO found on the provied Address!" 
                 oref : _   -> do
                                let tname      = npTokenName nparams
                                    redeemer   = (RP oref tname False)
                                    nft        = Value.singleton curSymbol tname 1 
                                    val        = (Ada.lovelaceValueOf 5_000_000) <> nft
                                    lookups    = Constraints.mintingPolicy policy  <>
                                                 Constraints.unspentOutputs utxos
                                    tx         = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData redeemer) nft <>
                                                 Constraints.mustSpendPubKeyOutput oref <> 
                                                 Constraints.mustPayToPubKey (PaymentPubKeyHash $ fromJust $ toPubKeyHash $ npReciever nparams) val
                                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                Contract.logInfo @String $ printf "Forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"giveMint" giveMint


test :: IO ()
test = runEmulatorTraceIO $ do
     let w1 = knownWallet 1
         w2 = knownWallet 2
     h1 <- activateContractWallet w1 endpoints
     h2 <- activateContractWallet w2 endpoints
     Extras.logInfo $ "Address w1 is:  " ++ show (knownWallet 1)
     callEndpoint @"giveMint" h1 $ NFTParams { npTokenName   = "Batch56nft1" 
                                         , npAddress     = mockWalletAddress w1
                                         , npReciever    = mockWalletAddress w2
                                         }
     void $ Emulator.waitNSlots 10
     Extras.logInfo $ "Address w2 is:  " ++ show (knownWallet 2)
     
     callEndpoint @"giveMint" h2 $ NFTParams { npTokenName = "Batch56nft2"
                                         , npAddress   = mockWalletAddress w2
                                         , npReciever  = mockWalletAddress w1   
                                         }
     s <- Emulator.waitNSlots 11
     Extras.logInfo $ "End of Simulation at slot " ++ show s