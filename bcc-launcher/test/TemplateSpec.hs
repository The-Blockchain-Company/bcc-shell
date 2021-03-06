{-| Test suite for Bcc.Shell.Template
|-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TemplateSpec
    ( templateSpec
    ) where

import           Bcc.Prelude
import           Data.Char (isAlphaNum)
import           System.Environment (lookupEnv, setEnv, unsetEnv)
import           System.IO.Error (userError)
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), elements, listOf1, suchThat)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Bcc.Shell.Template (substituteA)

-- | Pair of environment variable and its value
data EnvValue = EnvValue Text Text
    deriving Show

instance Arbitrary EnvValue where
    arbitrary = EnvValue <$> randomEnv <*> randomVal
      where
        randomEnv = toS <$> listOf1 (elements $ ['A' .. 'Z'] <> ['0' .. '9'] <> ['_'])
        randomVal = toS <$> listOf1 (arbitrary `suchThat` isAlphaNum)

-- | Test that @substituteA@ will substitute @${var}@ with @value@ by looking up the
-- environment variable
templateSpec :: Spec
templateSpec = modifyMaxSuccess (const 5000) $ do
    prop "should be able to perform env var substitution" $
        \(EnvValue var value)-> monadicIO $ do
            shouldBeValue <- run $ do
                subbed <- withEnv var value
                    (substituteA ("${" <> var <> "}") substituteVar)
                return $ toStrict subbed
            assert $ shouldBeValue == value
    prop "should throw error if substitution fails" $
        \(EnvValue var _value) -> monadicIO $ do
            (eShouldFail :: Either IOException Text) <- run . try $ do
                unsetEnv (toS var)
                let text = "${" <> var <> "}"
                subbed <- substituteA text substituteVar
                return $ toStrict subbed
            assert $ isLeft eShouldFail
  where
    substituteVar :: Text -> IO Text
    substituteVar var' = do
        mValue <- lookupEnv (toS var')
        case mValue of
            Nothing -> ioError $ userError "Environment variable not found!"
            Just value -> return $ toS value
    withEnv :: Text -> Text -> IO a -> IO a
    withEnv var value action = bracket
        (setEnv (toS var) (toS value))
        (const $ unsetEnv (toS var))
        (const action)
