module Main where

import           Bcc.Prelude
import           EnvironmentSpec (envSubstitutionSpec)
import           LauncherSMSpec (launcherSMSpec)
import           LauncherSpec (launcherSpec)
import           TemplateSpec (templateSpec)
import           Test.Hspec (describe, hspec)
import           UpdaterSpec (updaterSpec)

-- | Entry point for tests.
main :: IO ()
main = hspec $ do
    describe "Update spec" updaterSpec
    describe "Launcher spec" launcherSpec
    describe "LauncherSM spec" launcherSMSpec
    describe "Template spec" templateSpec
    describe "Env substitution spec" envSubstitutionSpec

