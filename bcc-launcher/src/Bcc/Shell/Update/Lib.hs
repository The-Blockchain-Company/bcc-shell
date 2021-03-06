{-| Update module
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bcc.Shell.Update.Lib
    ( UpdaterData (..)
    , RunUpdateFunc
    , runUpdater
    , runDefaultUpdateProcess
    -- * Intepretable language
    , UpdaterExists (..)
    , UpdaterCommand (..)
    , RemoveArchiveAfterInstall (..)
    , executeUpdater
    , isUpdaterRunOnWin
    , isUpdaterRunOnUnix
    , updaterExistsToBool
    -- * OS specific type/function
    , UpdateOSPlatform (..)
    , osToUpdateOSPlatform
    ) where

import           Bcc.Prelude

import           Distribution.System (OS (..), buildOS)
import           Prelude (String)

import           System.Directory (doesFileExist, removeFile)
import           System.Process (CreateProcess (..), StdStream (..), proc,
                                 waitForProcess, withCreateProcess)

#ifdef mingw32_HOST_OS
import qualified Data.Text as T
import           System.Environment (getExecutablePath)
import           System.Win32.Process (getCurrentProcessId)
#endif

import           Test.QuickCheck (Arbitrary (..), oneof)

import           Bcc.Shell.Launcher.Types (LoggingDependencies (..))


-- We need to add the check if the archive exists first!
-- /usr/bin/open -FW  /../klarity.pkg -- MAC
-- /bin/update-runner /../installer.sh -- LINUX
--                    ABS. PATH
-- Installer.bat      Installer.exe
-- WE GENERATE THIS!

-- | Runner path, what we use to run the update/installer,
-- arguments for the runner, and the actual update path for the installer.
data UpdaterData = UpdaterData
    { udUpdaterPath         :: !FilePath
    -- ^ Path of the updater/installer runner. Examples:
    -- - /usr/bin/open
    -- - /bin/update-runner
    -- - Installer.bat (that we generate)
    , udArgs                :: ![Text]
    -- ^ Arguments for the updater/installer.
    , udArchivePath         :: !FilePath
    -- ^ The update path of the update file. Examples:
    -- - /../klarity.pkg
    -- - /../installer.sh
    -- - Installer.exe (Found in the working directory)
    }

-- | On what platform are we running the update?
data UpdateOSPlatform
    = WinOS
    | UnixOS
    deriving (Eq, Show)

-- | Conversion of the build OS to our definition.
osToUpdateOSPlatform :: OS -> UpdateOSPlatform
osToUpdateOSPlatform Windows    = WinOS
osToUpdateOSPlatform _          = UnixOS

-- | The way we should run the process normally.
runDefaultUpdateProcess :: FilePath -> [String] -> IO ExitCode
runDefaultUpdateProcess path args = do
    let process = (proc path args)
            { std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    withCreateProcess process
        $ \_in _out _err ph -> waitForProcess ph

-- The function for executing the update.
type RunUpdateFunc = FilePath -> [String] -> IO ExitCode

-- | A flag for the existence of the updater file.
data UpdaterExists
    = UpdaterExists
    | UpdaterDoesntExist
    deriving (Eq, Show)

instance Arbitrary UpdaterExists where
    arbitrary = oneof [pure UpdaterExists, pure UpdaterDoesntExist]

-- | A simple isomorphic conversion from a more rich/descriptive type.
updaterExistsToBool :: UpdaterExists -> Bool
updaterExistsToBool UpdaterExists       = True
updaterExistsToBool UpdaterDoesntExist  = False

-- | A simple isomorphic conversion to a more rich/descriptive type.
boolToUpdaterExists :: Bool -> UpdaterExists
boolToUpdaterExists True    = UpdaterExists
boolToUpdaterExists False   = UpdaterDoesntExist

-- | The @Bool@ isomorphic type that signifies if we delete the archive/install
-- file or not.
data RemoveArchiveAfterInstall
    = RemoveArchiveAfterInstall
    | DoNotRemoveArchiveAfterInstall
    deriving (Eq, Show)

-- | The small language we use to distinct between execution.
data UpdaterCommand
    = WindowsRunUpdate !FilePath ![Text]
    | UnixRunUpdate !FilePath ![Text]
    | UpdaterFileMissing
    deriving (Eq, Show)

isUpdaterRunOnWin :: UpdaterCommand -> Bool
isUpdaterRunOnWin (WindowsRunUpdate _ _)    = True
isUpdaterRunOnWin _                         = False

isUpdaterRunOnUnix :: UpdaterCommand -> Bool
isUpdaterRunOnUnix (UnixRunUpdate _ _)  = True
isUpdaterRunOnUnix _                    = False

-- | Interpret the small language into the "real" semantics.
evaluateUpdaterCmdExitCode :: RunUpdateFunc -> UpdaterCommand -> IO ExitCode
evaluateUpdaterCmdExitCode runCommand = \case
    -- The update needs to be run on Windows.
    WindowsRunUpdate updaterPath args -> do
#ifdef mingw32_HOST_OS
        writeWindowsUpdaterRunner updaterPath
#endif
        runCommand updaterPath (map toS args)
    -- The update needs to be run on *nix.
    UnixRunUpdate updaterPath args -> do
        runCommand updaterPath (map toS args)
    -- The update file is missing. Maybe there is a more
    -- meaningful exit signal than this?
    UpdaterFileMissing      -> return $ ExitFailure 127

-- | Interpret the small language into the logging semantics.
evaluateUpdaterCmdLogging :: LoggingDependencies -> UpdaterCommand -> IO ()
evaluateUpdaterCmdLogging loggingDep = \case
    WindowsRunUpdate _ _    -> logInfo loggingDep $ "Running WIN update."
    UnixRunUpdate _ _       -> logInfo loggingDep $ "Running UNIX update."
    UpdaterFileMissing      -> logError loggingDep $ "The updater file is missing!"

-- | Run the update system
--
-- For UNIX system:
--
-- Check that @udPath@ exists, then run the command @udPath udArgs udArchivePath@
--
-- For Windows:
--
-- Check that @udPath@ exists, but instead of running the command directly, you
-- first have to generate a @.bat@ file which will act as a script.
-- After it being generated, you run that script.
runUpdater
    :: LoggingDependencies
    -> RemoveArchiveAfterInstall
    -> RunUpdateFunc
    -> UpdaterData
    -> IO ExitCode
runUpdater loggingDep removeArchive runCommand  updaterData = do

    -- The update installation.
    let archivePath     = udArchivePath updaterData

    -- Does the actual archive, the update installation, called here "updater", exists?
    updaterExist <- boolToUpdaterExists <$> doesFileExist archivePath

    logInfo loggingDep $ "Does update installation exist: " <> show updaterExist

    let currentBuildOS :: UpdateOSPlatform
        currentBuildOS = osToUpdateOSPlatform buildOS

    let updaterCommand :: UpdaterCommand
        updaterCommand = executeUpdater currentBuildOS updaterExist updaterData

    -- Log out the results.
    evaluateUpdaterCmdLogging loggingDep updaterCommand

    -- Actually execute the commands.
    exitCode <- evaluateUpdaterCmdExitCode runCommand updaterCommand

    -- The handling of the final exit code of the updater.
    case exitCode of
        -- If the update is a success, them remove the installer file.
        ExitSuccess -> do
            let removeArchivePath = removeArchive == RemoveArchiveAfterInstall
            doesArchiveFileExist <- doesFileExist archivePath

            when (removeArchivePath && doesArchiveFileExist) $
                removeFile archivePath

            return $ ExitSuccess
        -- Otherwise, return an error.
        _           -> return exitCode -- Maybe exitWith?

-- | Pure execution of @UpdaterCommand@.
executeUpdater :: UpdateOSPlatform -> UpdaterExists -> UpdaterData -> UpdaterCommand
executeUpdater buildOS' updaterExist updaterData = do

    let updaterPath = udUpdaterPath updaterData
    let args        = map toS $ udArgs updaterData
    let archivePath = udArchivePath updaterData

    if updaterExist == UpdaterExists
        then
            case buildOS' of
                WinOS   -> WindowsRunUpdate updaterPath (toS archivePath:args)
                UnixOS  -> UnixRunUpdate    updaterPath (toS archivePath:args)
        else UpdaterFileMissing

-- | Create @.bat@ file on given @FilePath@
--
-- https://github.com/The-Blockchain-Company/bcc-sl/blob/develop/tools/src/launcher/Main.hs#L585
--
-- The installer cant write to bcc-launcher.exe while it is running
-- so you must fully stop launcher before you can start the installer.
-- Because of this, we need a @.bat@ file which will run the update procedure and
-- re-launch the launcher.
-- Only Windows has this problem.
#ifdef mingw32_HOST_OS
writeWindowsUpdaterRunner :: FilePath -> IO ()
writeWindowsUpdaterRunner runnerPath = do
    exePath         <- getExecutablePath -- (TODO): Check that it returns absolute path!
    launcherArgs    <- getArgs

    selfPid         <- getCurrentProcessId
    writeFile (toS runnerPath) $ T.unlines
    -- What info can this file supply if it fails?
    -- How can you make this scream if it fails
    -- Checksum of the updater exe?
    -- Only then run it
        [ "TaskKill /PID "<> show selfPid <>" /F"
        -- Run updater
        , "%*"
        -- Delete updater
        , "del %1"
        -- Run launcher again
        , "start \"bcc launcher\" /b " <> (quote $ toS exePath) <> " "
            <> (T.unwords $ map (quote . toS) launcherArgs)
        -- Delete the bat file
        , "(goto) 2>nul & del \"%~f0\""
        ]
  where
    quote :: Text -> Text
    quote str = "\"" <> str <> "\""
    -- str = a"b
    -- possible inject attack
#endif

