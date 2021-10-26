module BccWalletIPCSpec
    ( spec
    ) where

import           Bcc.Prelude

import           Test.Hspec (Spec, describe, it, pending)

spec :: Spec
spec = describe "BccWalletIPC" $ do
    it "should reply with the port when asked" $ do
        pending


--spec = describe "BccWalletIPC" $ do
--    it "should reply with the port when asked" $ do
--        let port = 42 :: Int
--        let testScript = "test/js/mock-klarity.js"
--        (_, _, _, ph) <- createProcess (proc "node" [testScript, show port])
--        waitForProcess ph `shouldReturn` ExitSuccess
