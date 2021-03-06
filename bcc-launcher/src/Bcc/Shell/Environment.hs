{-# LANGUAGE OverloadedStrings #-}

module Bcc.Shell.Environment
  ( SubstitutionError(..)
  , substituteEnvVars
  ) where

import           Bcc.Prelude

import           Bcc.Shell.Template (substituteA)
import           Data.Yaml (Value (..))
import           Prelude (Show (..))
import           System.Environment (lookupEnv)

-- | Substitute envrionment variable with value of its name.
--
-- @
-- runExceptT $ substituteEnvVarsText "Hello ${user}"
-- Right "Hello hiroto"
-- @
substituteEnvVarsText :: Text -> ExceptT SubstitutionError IO Text
substituteEnvVarsText text = toStrict <$> (substituteA text expandVariable)

-- type ContextA f = Text -> f Text
-- | Return the value of the environment variable @var@, or throw error if there
-- is no such value.
expandVariable :: Text -> ExceptT SubstitutionError IO Text
expandVariable var = do
    mValue <- liftIO $ lookupEnv (toS var)
    case mValue of
        Nothing    -> throwError $ FailedToLookupEnv var
        Just value -> return $ toS value

data SubstitutionError
  = FailedToLookupEnv Text
  -- ^ Failed to lookup environment variable
  deriving (Eq)

instance Show SubstitutionError where
  show (FailedToLookupEnv env) = "Failed to lookup environment variable: " <> toS env

-- | Given an Aeson 'Value', parse and substitute environment variables in all
-- 'String' objects.
substituteEnvVars :: Value -> ExceptT SubstitutionError IO Value
substituteEnvVars (String text) = String <$> substituteEnvVarsText text
substituteEnvVars (Array xs)    = Array  <$> traverse substituteEnvVars xs -- Traversing through array
substituteEnvVars (Object o)    = Object <$> traverse substituteEnvVars o -- Traversing through object
substituteEnvVars x             = return x
