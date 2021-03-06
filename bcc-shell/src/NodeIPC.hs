#TODOCreate spec pdf
{-| Node IPC module. For details please read the spec:

<https://github.com/The-Blockchain-Company/bcc-shell/blob/develop/specs/BccShellSpec.pdf>
-}

module Bcc.Shell.NodeIPC
    (-- * Data types
      Port(..)
    , MsgIn(..)
    , MsgOut(..)
    , ReadHandle(..)
    , WriteHandle(..)
     -- * IPC protocol
    , ProtocolDuration (..)
    , startNodeJsIPC
    , startIPC
    , handleIPCProtocol
    , clientIPCListener
    , testStartNodeIPC
    , ServerHandles (..)
    , ClientHandles (..)
    , closeFullDuplexAnonPipesHandles
    , createFullDuplexAnonPipesHandles
    , bracketFullDuplexAnonPipesHandles
    , serverReadWrite
     -- ** Exceptions
    , NodeIPCError(..)
    , MessageSendFailure(..)
    , MessageException(..)
     -- * Used for testing
    , sendMessage
    , readMessage
    , exampleWithFD
    , exampleServerWithProcess
    , getReadWriteHandles
    , getHandleFromEnv
    -- * Predicates
    , isIPCError
    , isHandleClosed
    , isUnreadableHandle
    , isUnwritableHandle
    , isNodeChannelCannotBeFound
    ) where

import           Bcc.Shell.NodeIPC.Lib (ClientHandles (..),
                                            MessageSendFailure (..), MsgIn (..),
                                            MsgOut (..), NodeIPCError (..),
                                            Port (..), ProtocolDuration (..),
                                            ServerHandles (..),
                                            bracketFullDuplexAnonPipesHandles,
                                            clientIPCListener,
                                            closeFullDuplexAnonPipesHandles,
                                            createFullDuplexAnonPipesHandles,
                                            getHandleFromEnv, handleIPCProtocol,
                                            isHandleClosed, isIPCError,
                                            isNodeChannelCannotBeFound,
                                            isUnreadableHandle,
                                            isUnwritableHandle, serverReadWrite,
                                            startIPC, startNodeJsIPC,
                                            testStartNodeIPC)
import           Bcc.Shell.NodeIPC.Message (MessageException (..),
                                                ReadHandle (..),
                                                WriteHandle (..), readMessage,
                                                sendMessage)
import           Bcc.Shell.NodeIPC.ServerExample (exampleServerWithProcess,
                                                      exampleWithFD,
                                                      getReadWriteHandles)
