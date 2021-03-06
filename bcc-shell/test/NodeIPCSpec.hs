{-| Module testing Node IPC
-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NodeIPCSpec
    ( nodeIPCSpec
    ) where

import           Bcc.Prelude

import           Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Encoding as Text
import           GHC.IO.Handle (hIsOpen)
import           System.IO (hClose)
import           System.IO.Error (eofErrorType, mkIOError, IOError)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Property)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Bcc.Shell.NodeIPC (MessageException,
                                        MessageSendFailure (..), MsgIn (..),
                                        MsgOut (..), NodeIPCError (..),
                                        Port (..), ProtocolDuration (..),
                                        ReadHandle (..), WriteHandle (..),
                                        getReadWriteHandles, isHandleClosed,
                                        isIPCError, isNodeChannelCannotBeFound,
                                        isUnreadableHandle, isUnwritableHandle,
                                        readMessage, sendMessage, startIPC,
                                        startNodeJsIPC, testStartNodeIPC)

-- | Test spec for node IPC
nodeIPCSpec :: Spec
nodeIPCSpec = do
    describe "Message passing" $ do
        describe "sendMessage/readMessage" $ do
            prop "should be able to send MsgIn messages using handle pairs" $
                \(randomMsg :: MsgIn) -> testMessage randomMsg
            prop "should be able to send MsgOut messages using handle pairs" $
                \(randomMsg :: MsgOut) -> testMessage randomMsg
            prop "should throw MessageException when incorrect message is sent" $
            -- Send random MsgOut, but try to decode it as MsgIn which would fail
                \(randomMsg :: MsgOut) -> monadicIO $ do
                    eResult <- run $ try $ do
                        (readHndl, writeHndl) <- getReadWriteHandles
                        sendMessage writeHndl randomMsg
                        readMessage readHndl :: (IO MsgIn)
                    assert $ isLeft (eResult :: Either MessageException MsgIn)

    describe "startIPC" $ do
        modifyMaxSuccess (const 1000) $ prop "model based testing" $
            -- Have both MsgIn and MsgOut in order to test failing cases
            \(eMsg :: Either MsgOut MsgIn) (randomPort :: Port) -> monadicIO $ do
                response <- run $ either
                    (testStartNodeIPC randomPort)
                    (testStartNodeIPC randomPort)
                    eMsg
                assert $ response == (Started, modelResponse randomPort eMsg)

        prop "should return Started and ShutdownInitiated when client sends message 'Shutdown'" $ monadicIO $ do
            result <- run $ try $ testStartNodeIPC port Shutdown
            assert $ isLeft (result :: Either IOError (MsgOut, MsgOut))
            --assert $ started == Started
            --assert $ pong    == ShutdownInitiated

        describe "Exceptions" $ do
            it "should throw NodeIPCError when closed handle is given" $ monadicIO $ do
                eResult <- run $ do
                    (readHandle, writeHandle) <- getReadWriteHandles
                    closedReadHandle <- (\(ReadHandle hndl) -> hClose hndl >> return (ReadHandle hndl)) readHandle
                    startIPC SingleMessage closedReadHandle writeHandle port
                assert $ isLeft (eResult :: Either NodeIPCError ())
                whenLeft eResult $ \exception -> assert $ isHandleClosed exception

            it "should throw NodeIPCError when unreadable handle is given" $ monadicIO $ do
                eResult <- run $ do
                    (readHandle, writeHandle) <- getReadWriteHandles
                    let (unReadableHandle, _) = swapHandles readHandle writeHandle
                    startIPC SingleMessage unReadableHandle writeHandle port
                assert $ isLeft (eResult :: Either NodeIPCError ())
                whenLeft eResult $ \exception -> assert $ isUnreadableHandle exception

            it "should throw NodeIPCError when unwritable handle is given" $ monadicIO $ do
                eResult <- run $ do
                    (readHandle, writeHandle) <- getReadWriteHandles
                    let (_, unWritableHandle) = swapHandles readHandle writeHandle
                    startIPC SingleMessage readHandle unWritableHandle port
                assert $ isLeft (eResult :: Either NodeIPCError ())
                whenLeft eResult $ \exception -> assert $ isUnwritableHandle exception

        describe "Resource cleanup" $ do
            it "should throw NodeIPCError when IOError is being thrown" $ monadicIO $ do
                eResult <- run $ do
                    (as, _, _) <- ipcTest
                    let ioerror = mkIOError eofErrorType "Failed with eofe" Nothing Nothing
                    cancelWith as ioerror
                    wait as
                assert $ isLeft (eResult :: Either NodeIPCError ())
                whenLeft eResult $ \exception -> assert $ isIPCError exception

            it "should close used handles when exception is being thrown" $ monadicIO $ do
                handlesClosed <- run $ do
                    (as, readHandle, writeHandle) <- ipcTest
                    let ioerror = mkIOError eofErrorType "Failed with eofe" Nothing Nothing
                    cancelWith as ioerror
                    areHandlesClosed readHandle writeHandle
                assert handlesClosed

            prop "should close used handles when the process is finished" $
                \(msg :: MsgIn) -> monadicIO $ do
                    handlesClosed <- run $ do
                        (clientReadHandle, clientWriteHandle) <- getReadWriteHandles
                        (serverReadHandle, serverWriteHandle) <- getReadWriteHandles
                        as <- async $ startIPC SingleMessage serverReadHandle clientWriteHandle port
                        let readClientMessage = readMessage clientReadHandle
                            sendServer        = sendMessage serverWriteHandle
                        _ <- readClientMessage
                        sendServer msg
                        (_ :: MsgOut) <- readClientMessage
                        _  <- wait as
                        areHandlesClosed serverReadHandle clientWriteHandle
                    assert handlesClosed

    describe "startNodeJsIPC" $
        it "should throw NodeIPCError when it is not spawned by NodeJS process" $ monadicIO $ do
            eResult <- run $ startNodeJsIPC SingleMessage port
            assert $ isLeft (eResult :: Either NodeIPCError ())
            whenLeft eResult $ \exception -> assert $ isNodeChannelCannotBeFound exception
  where
    port :: Port
    port = Port 8090

    swapHandles :: ReadHandle -> WriteHandle -> (ReadHandle, WriteHandle)
    swapHandles (ReadHandle rHandle) (WriteHandle wHandle) = (ReadHandle wHandle, WriteHandle rHandle)

    -- Check whether both handles are closed
    areHandlesClosed :: ReadHandle -> WriteHandle -> IO Bool
    areHandlesClosed (ReadHandle readHandle) (WriteHandle writeHandle) = do
        readIsOpen  <- hIsOpen readHandle
        writeIsOpen <- hIsOpen writeHandle
        return $ not $ and [readIsOpen, writeIsOpen]

    ipcTest :: IO (Async (Either NodeIPCError ()), ReadHandle, WriteHandle)
    ipcTest = do
        (clientReadHandle, clientWriteHandle) <- getReadWriteHandles
        (serverReadHandle, _)                 <- getReadWriteHandles

        as <- async $ startIPC SingleMessage serverReadHandle clientWriteHandle port
        (_ :: MsgOut) <- readMessage clientReadHandle
        return (as, serverReadHandle, clientWriteHandle)

-- | Test if given message can be send and recieved using 'sendMessage', 'readMessage'
testMessage :: (FromJSON msg, ToJSON msg, Eq msg) => msg -> Property
testMessage msg = monadicIO $ do
    response <- run $ do
        (readHndl, writeHndl) <- getReadWriteHandles
        sendMessage writeHndl msg
        readMessage readHndl

    assert $ response == msg


whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) f = f x
whenLeft _        _ = pure ()

-- Try to predict the @MsgOut@ with given @Port@ and @Either MsgOut MsgIn@
modelResponse :: Port -> Either MsgOut MsgIn -> MsgOut
modelResponse (Port portNumber) = \case
    Left msgOut ->
        let errorMessage = "Failed to decode given blob: " <> Text.decodeUtf8 (LBS.toStrict $ encode msgOut)
        in MessageOutFailure $ ParseError errorMessage
    Right QueryPort ->
        ReplyPort portNumber
    Right Ping ->
        Pong
    Right Shutdown ->
        ShutdownInitiated
    Right (MessageInFailure f) ->
        MessageOutFailure f
