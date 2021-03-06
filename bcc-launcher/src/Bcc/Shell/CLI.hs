module Bcc.Shell.CLI
    ( getLauncherOptions
    , decodeLauncherOption
    , LauncherOptionPath(..)
    , LauncherOptionError(..)
    , setupEnvVars
    -- * config parsing
    , getDefaultConfigPath
    , launcherArgsParser
    ) where

import           Bcc.Prelude

import           Bcc.Shell.Environment (SubstitutionError,
                                            substituteEnvVars)
import           Control.Monad.Except (liftEither)
import           Data.Aeson (Result (..), fromJSON)
import           Data.Yaml (ParseException, decodeFileEither)
import           Options.Applicative (Parser, help, long, metavar, short,
                                      strOption, value)

import           System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import           System.Environment (getExecutablePath, setEnv)
import           System.FilePath (takeDirectory, (</>))

import           Bcc.Shell.Configuration (LauncherOptions)
import           Bcc.Shell.Launcher.Types (LoggingDependencies (..))

-- | Path to launcher-config.yaml file
newtype LauncherOptionPath = LauncherOptionPath
    { getLauncherOptionPath  :: FilePath
    } deriving (Eq, Show)

-- | Default path to the launcher-config.yaml file
--
-- This used on both windows and mac.
--
-- this will enable the launcher to load launcher-config.yaml from the same
-- directory as the bcc-launcher binary
getDefaultConfigPath :: IO FilePath
getDefaultConfigPath = do
    launcherDir <- takeDirectory <$> getExecutablePath
    pure $ launcherDir </> "launcher-config.yaml"

-- | CLI for @LauncherOptionPath@
launcherArgsParser :: FilePath -> Parser LauncherOptionPath
launcherArgsParser defaultPath = LauncherOptionPath <$> strOption (
    short   'c' <>
    long    "config" <>
    help    ("Path to the launcher configuration file. If not provided, it'll\
        \ instead use\n" <> defaultPath) <>
    metavar "PATH" <>
    value defaultPath )

data LauncherOptionError
    = FailedToParseLauncherOption Text
    -- ^ Failed to convert yaml @Value@ into @LauncherOption@ type
    | FailedToDecodeFile ParseException
    -- ^ Failed to decode yaml file
    | SubstitutionFailed SubstitutionError
    -- ^ Failed to perform env var substitution
    deriving Show

-- | Command line argument parser for @LauncherOptions@
getLauncherOptions
    :: LoggingDependencies
    -> LauncherOptionPath
    -> IO (Either LauncherOptionError LauncherOptions)
getLauncherOptions logDeps loPath = do

    setupEnvVars loPath

    eLauncherOption <- decodeLauncherOption logDeps loPath
    case eLauncherOption of
        Left decodeError      -> return . Left $ decodeError
        Right launcherOptions -> return . Right $ launcherOptions

-- There a lot of @withExceptT@ 's since all these function returns different
-- types of @Either@ so I have to make the types align
decodeLauncherOption
    :: LoggingDependencies
    -> LauncherOptionPath
    -> IO (Either LauncherOptionError LauncherOptions)
decodeLauncherOption logDeps loPath = runExceptT $ do

        decodedVal <- withExceptT FailedToDecodeFile .
            ExceptT . decodeFileEither . getLauncherOptionPath $ loPath

        substituted <- withExceptT SubstitutionFailed .
            substituteEnvVars $ decodedVal

        lift $ logNotice logDeps $ "Launcher substituted ENV variables: " <> show substituted

        parsed <- withExceptT FailedToParseLauncherOption .
            liftEither . resultToEither . fromJSON $ substituted

        return parsed

-- Set environment variables that we need in order for launcher to perform
-- env var substitution
setupEnvVars :: LauncherOptionPath -> IO ()
setupEnvVars (LauncherOptionPath configPath) = do
    bccwalletDir <- takeDirectory <$> getExecutablePath
    setEnv "KLARITY_INSTALL_DIRECTORY" bccwalletDir
    getXdgDirectory XdgData "" >>= setEnv "XDG_DATA_HOME"
    setEnv "LAUNCHER_CONFIG" configPath

-- | Convert @Result a@ type into @Either Text a@
resultToEither :: Result a -> Either Text a
resultToEither (Success a) = Right a
resultToEither (Error str) = Left (toS str)

