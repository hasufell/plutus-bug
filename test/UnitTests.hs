{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE BlockArguments        #-}

module UnitTests (tests) where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras.Log as Log
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), check)
import           Plutus.Contract.Trace      as Emulator
import           Plutus.Contract.Test       as Test
import           Data.Aeson.Types           as JSON
import qualified Prelude
import           Wallet.Emulator.MultiAgent  (eteEvent)
import           Plutus.Trace.Emulator.Types (_ContractLog, cilMessage)

import           Test.Tasty

import           PlutusBug

-- | Test configuration: three wallets with 100 ADA on each of them.
--
defaultEmCfg :: Emulator.EmulatorConfig
defaultEmCfg = Emulator.EmulatorConfig (Left $ Map.fromList [(w1, v), (w2, v), (w3, v)]) def def
  where
    v :: Ledger.Value
    v = Ada.lovelaceValueOf 100_000_000

-- | Shortage for `checkPredicateOptions`: call this function with default params.
--
check :: Prelude.String -> TracePredicate -> EmulatorTrace () -> TestTree
check = checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ defaultEmCfg & minLogLevel .~ Log.Debug)

contract' :: Contract () PlutusBug.Schema Text ()
contract' = PlutusBug.contract


tests :: TestTree
tests = testGroup "unit tests"
    [ testLock
    ]


testLock :: TestTree
testLock = check "publish"
    (     walletFundsChange w1 (Ada.lovelaceValueOf (-80_000_000))
     .&&. walletFundsChange w2 (Ada.lovelaceValueOf 0)
     .&&. walletFundsChange w3 (Ada.lovelaceValueOf 0)
    )
    do
        hdl1 <- activateContractWallet w1 contract'
        void $ Emulator.waitNSlots 2
        callEndpoint @"publish" hdl1 (1, Ada.lovelaceValueOf 50_000_000)
        void $ Emulator.waitNSlots 2
        callEndpoint @"publish" hdl1 (2, Ada.lovelaceValueOf 30_000_000)
        void $ Emulator.waitNSlots 2

