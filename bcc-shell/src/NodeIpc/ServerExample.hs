{-| This module provides an example of how NodeIPC works.
--
--
-- We want server/client to read only the messages that each should care about
-- In order to realize this, we need two proccesses with each of them providing
-- read/write handle.
--
-- These processes will then pass each others handle respectively and use it to
-- communicate with each other.
--
-- Server will take client's write handle and server's read handle.
--
-- Client will take server's write handle and client's read handle.
--
-- This allows the two proccesses to send the message to the other while
-- reading the response that other had sent.
-}

module Bcc.Shell.NodeIPC.ServerExample
    ( exampleWithFD
    , exampleServerWithProcess
    -- * For testing
    , getReadWriteHandles
    ) where

import           Bcc.Prelude

import           Bcc.Shell.NodeIPC.Lib (MsgIn (..), MsgOut (..),
                                            NodeIPCError (..), Port (..),
                                            ProtocolDuration (..), startIPC)
import           Bcc.Shell.NodeIPC.Message (ReadHandle (..),
                                                WriteHandle (..), readMessage,
                                                sendMessage)
import           GHC.IO.Handle.FD (fdToHandle)
import           System.Environment (setEnv, unsetEnv)
import           System.IO (BufferMode (..), hClose, hSetBuffering)
import           System.Process (CreateProcess (..), StdStream (..), createPipe,
                                 createPipeFd, proc, withCreateProcess)

-- | Create a pipe for interprocess communication and return a
-- ('ReadHandle', 'WriteHandle') Handle pair.
getReadWriteHandles :: IO (ReadHandle, WriteHandle)
getReadWriteHandles = do
    (readHndl, writeHndl) <- createPipe
    hSetBuffering readHndl LineBuffering
    hSetBuffering writeHndl LineBuffering
    let readHandle  = ReadHandle readHndl
    let writeHandle = WriteHandle writeHndl
    return (readHandle, writeHandle)

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

nodePort :: Port
nodePort = Port 8090

-- | Example using file descriptor
exampleWithFD :: MsgIn -> IO (MsgOut, MsgOut)
exampleWithFD msgin = do

    (serverReadHandle, serverWriteHandle) <- getReadWriteHandles

    (_, responses) <- do
        (clientReadHandle, clientWriteHandle) <- getReadWriteHandles
        -- Send message to client
        sendMessage clientWriteHandle msgin
        startIPC SingleMessage clientReadHandle serverWriteHandle nodePort
        `concurrently`
        receieveMessages serverReadHandle

    return responses

-- | Example of an IPC server that is using haskell executable as an server.
--
-- This will be the server, the one which sends the message (such as @Ping@, @QueryPort@)
-- to get the response from the client.
-- The client is executed via @stack exec node-ipc haskell@
exampleServerWithProcess :: MsgIn -> IO (Either NodeIPCError (MsgOut, MsgOut))
exampleServerWithProcess msg = bracket acquire restore (action msg)
  where
    acquire :: IO (ReadHandle, Handle)
    acquire = do
        (rFd, wFd) <- createPipeFd
        -- Set the write file descriptor to the envrionment variable
        -- the client will look this up, and use it to talk the server
        setEnv "NODE_CHANNEL_FD" (show wFd)
        readHandle  <- ReadHandle <$> fdToHandle rFd
        -- Since closeFd only exists in 'unix' library,
        -- the only way to close this Fd is to convert it into Handle and apply
        -- hClose to it
        writeHandle <- fdToHandle wFd
        return (readHandle, writeHandle)

    restore :: (ReadHandle, Handle) -> IO ()
    restore ((ReadHandle rHandle), wHandle) = do
        hClose rHandle
        hClose wHandle
        unsetEnv "NODE_CHANNEL_FD"

    action :: MsgIn
           -> (ReadHandle, Handle)
           -> IO (Either NodeIPCError (MsgOut, MsgOut))
    action msgin (readHandle, _) = do
        withCreateProcess (proc "stack" ["exec", "node-ipc", "haskell"])
            { std_in = CreatePipe } $
                \mStdIn _ _ _ ->
                    case mStdIn of
                      Nothing ->
                        panic "Bcc.Shell.NodeIPC.ServerExample.exampleServerWithProcess: Nothing"
                      Just stdIn -> do
                        sendMessage (WriteHandle stdIn) msgin
                        Right <$> receieveMessages readHandle


-- | Read message wigh given 'ReadHandle'
receieveMessages :: ReadHandle -> IO (MsgOut, MsgOut)
receieveMessages serverReadHandle = do
    let readServerMessage :: IO MsgOut
        readServerMessage = readMessage serverReadHandle
    started <- readServerMessage -- Started
    reply   <- readServerMessage -- Reply
    return (started, reply)

