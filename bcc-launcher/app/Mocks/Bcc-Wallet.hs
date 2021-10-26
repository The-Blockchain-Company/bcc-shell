{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bcc.Prelude
import System.Exit (exitWith)

seconds :: Int
seconds = 1000000

main :: IO ExitCode
main = do
    putTextLn $ "Starting Klarity"
    threadDelay $ 5 * seconds
    putText $ "Exiting for update"
    exitWith $ ExitFailure 20