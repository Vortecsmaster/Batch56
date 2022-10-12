{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}

module CustomRedeemer where

import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger             (TxOutRef, TokenName)

data RedeemerParam = RP
                   { rpOref :: TxOutRef
                   , rpTname :: TokenName
                   , rpGiveOrSell :: Bool
                   }  

PlutusTx.makeIsDataIndexed ''RedeemerParam [('RP,0)]
