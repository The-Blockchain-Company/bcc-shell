{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2021 The-Blockchain-Company
--
-- Klarity <-> Wallet child process port discovery protocol.
-- Provides a mechanism for Klarity to discover what port the klarity
-- server is listening on.
--
-- See <https://nodejs.org/api/child_process.html#child_process_child_process_spawn_command_args_options>
-- for more information about the message protocol.

module Bcc.Shell.BccWalletIPC
    ( bccwalletIPC
    ) where

import           Bcc.Prelude

import           Bcc.Shell.NodeIPC.General (NodeChannelError (..),
                                                NodeChannelFinished (..),
                                                runNodeChannel,
                                                setupNodeChannel)
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object,
                             withObject, (.:), (.=))
import           Data.Text (Text)

-- | Messages sent from Klarity -> klarity
data MsgIn = QueryPort
    deriving (Show, Eq)

-- | Messages sent from klarity -> Klarity
data MsgOut = Started | ReplyPort Int | ParseError Text
    deriving (Show, Eq)

instance FromJSON MsgIn where
    parseJSON = withObject "MsgIn" $ \v -> do
        (_ :: [()]) <- v .: "QueryPort"
        pure QueryPort

instance ToJSON MsgOut where
    toJSON Started        = object [ "Started" .= Array mempty ]
    toJSON (ReplyPort p)  = object [ "ReplyPort" .= p ]
    toJSON (ParseError e) = object [ "ParseError" .= e ]

-- | Start up the Klarity IPC process. It's called 'bccwalletIPC', but this
-- could be any nodejs program that needs to start klarity. All it does
-- is reply with a port number when asked, using a very nodejs-specific IPC
-- method.
--
-- If the IPC channel was successfully set up, this function won't return until
-- the parent process exits. Otherwise, it will return immediately. Before
-- returning, it will log an message about why it has exited.
--
-- TODO(KS): If you want to use TRACE here, you need to provide the trace functions
-- as params OR provide a record of trace functions.
bccwalletIPC
    :: Int
    -- ^ Port number to send to Klarity
    -> IO ()
bccwalletIPC port = setupNodeChannel >>= \case
    Right chan -> do
        putTextLn "Klarity IPC server starting"
        runNodeChannel (pure . msg) action chan >>= \case
            Left (NodeChannelFinished err) ->
                putTextLn $ "Klarity IPC finished for this reason: " <> show err
            Right () -> putTextLn "Unreachable code"
    Left NodeChannelDisabled -> do
        putTextLn "Klarity IPC is not enabled."
        sleep
    Left (NodeChannelBadFD err) ->
        putTextLn $ "Problem starting Klarity IPC: " <> show err
  where
    -- How to respond to an incoming message, or when there is an incoming
    -- message that couldn't be parsed.
    msg (Right QueryPort) = Just (ReplyPort port)
    msg (Left e)          = Just (ParseError e)

    -- What to do in context of runNodeChannel
    action :: (MsgOut -> IO ()) -> IO ()
    action send = send Started >> sleep

    sleep = forever $ threadDelay maxBound
