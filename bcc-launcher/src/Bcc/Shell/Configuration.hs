{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bcc.Shell.Configuration
    ( WalletArguments (..)
    , BccWalletBin (..)
    , LauncherOptions (..)
    , ConfigurationOptions (..)
    -- * Getters
    , getUpdaterData
    , getDPath
    -- * Setting up working directory
    , setWorkingDirectory
    ) where

import           Bcc.Prelude

import           Data.Time.Units (Microsecond, fromMicroseconds)
import           Data.Yaml (FromJSON (..), withObject, (.:), (.:?))
import           System.Directory (doesDirectoryExist, setCurrentDirectory)

import           Bcc.Shell.Update.Lib (UpdaterData (..))
import           Bcc.X509.Configuration (TLSConfiguration)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------
-- | Arguments that will be used to execute the wallet
newtype WalletArguments = WalletArguments
    { getWalletArguments :: [Text]
    } deriving (Eq, Show)

-- | We define the instance on it's wrapped type.
instance Semigroup WalletArguments where
    (<>) = \wArgs1 wArgs2 -> WalletArguments $ getWalletArguments wArgs1 <> getWalletArguments wArgs2

-- | Path to wallet executable
newtype BccWalletBin = BccWalletBin
    { getBccWalletBin :: Text
    } deriving (Eq, Show)

-- Todo: Add haddock comment for each field
-- | Launcher options
data LauncherOptions = LauncherOptions
    { loConfiguration       :: !(Maybe ConfigurationOptions)
    , loTlsPath             :: !(Maybe FilePath)
    , loTlsConfig           :: !(Maybe TLSConfiguration)
    , loUpdaterPath         :: !FilePath
    , loUpdaterArgs         :: ![Text]
    , loUpdateArchive       :: !FilePath
    , loBccWalletBin         :: !FilePath
    , loWorkingDirectory    :: !FilePath
    , loStateDir            :: !FilePath
    -- On WIN it should set this directory as current.
    } deriving (Show, Generic)

instance FromJSON LauncherOptions where
    parseJSON = withObject "LauncherOptions" $ \o -> do

        bccwalletBin         <- o .: "bccwalletBin"
        updaterPath         <- o .: "updaterPath"
        updaterArgs         <- o .: "updaterArgs"
        updateArchive       <- o .: "updateArchive"
        configuration       <- o .:? "configuration"
        tlsPath             <- o .:? "tlsPath"
        tlsConfig           <- o .:? "tlsConfig"
        workingDir          <- o .: "workingDir"
        stateDir            <- o .: "stateDir"

        pure $ LauncherOptions
            configuration
            tlsPath
            tlsConfig
            updaterPath
            updaterArgs
            updateArchive
            bccwalletBin
            workingDir
            stateDir

-- | Configuration yaml file location and the key to use. The file should
-- parse to a MultiConfiguration and the 'cfoKey' should be one of the keys
-- in the map.
data ConfigurationOptions = ConfigurationOptions
    { cfoFilePath    :: !FilePath
    , cfoKey         :: !Text
    , cfoSystemStart :: !(Maybe Timestamp)
    -- ^ An optional system start time override. Required when using a
    -- testnet genesis configuration.
    , cfoSeed        :: !(Maybe Integer)
    -- ^ Seed for secrets generation can be provided via CLI, in
    -- this case it overrides one from configuration file.
    } deriving (Show)

-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
-- Amount of microseconds since Jan 1, 1970 UTC.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Show, Num, Eq, Ord, Enum, Real, Integral, Typeable, Generic)

instance FromJSON ConfigurationOptions where
    parseJSON = withObject "ConfigurationOptions" $ \o -> do
        path            <- o .: "filePath"
        key             <- o .: "key"
        systemStart     <- (Timestamp . fromMicroseconds . (*) 1000000) <<$>> o .:? "systemStart"
        seed            <- o .:? "seed"
        pure $ ConfigurationOptions path key systemStart seed

--------------------------------------------------------------------------------
-- These functions will take LauncherOptions as an argument and put together
-- that data so that it can be used
--------------------------------------------------------------------------------

-- | Create @UpdaterData@ with given @LauncherOptions@
getUpdaterData :: LauncherOptions -> UpdaterData
getUpdaterData lo =
    let updaterPath     = loUpdaterPath lo
        updaterArgs     = loUpdaterArgs lo
        archivePath     = loUpdateArchive lo
    in UpdaterData updaterPath updaterArgs archivePath

-- | Return BccWalletBin
getDPath :: LauncherOptions -> BccWalletBin
getDPath lo = BccWalletBin $ toS $ loBccWalletBin lo

-- | Set working directory to given @FilePath@, return false if directory does
-- not exist
setWorkingDirectory :: FilePath -> IO Bool
setWorkingDirectory filePath = do
    directoryExists <- doesDirectoryExist filePath
    when directoryExists $ setCurrentDirectory filePath
    return directoryExists

