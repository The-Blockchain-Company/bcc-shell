module Main where

import           Bcc.Prelude

import           Test.Hspec (describe, hspec)

import qualified BccWalletIPCSpec as BccWalletIPC
import           NodeIPCSMSpec (nodeIPCSMSpec)
import           NodeIPCSpec (nodeIPCSpec)

-- | Entry point for tests.
main :: IO ()
main = hspec $ do
    describe "NodeIPC state machine" nodeIPCSMSpec
    describe "NodeIPC" nodeIPCSpec
    describe "BccWalletIPC" BccWalletIPC.spec

