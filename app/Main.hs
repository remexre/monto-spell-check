{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (parser)
import Data.Aeson (Value)
import Data.Map (singleton)
import Language.Aspell (spellChecker)
import Network.Monto.Common.Types
  ( ProductDescriptor(..)
  , ProductName(..)
  )
import Network.Monto.Service
  ( Handler
  , runService
  )
import Options.Applicative (execParser)
import Spellcheck (spellcheck)
import System.Exit (exitFailure)

main :: IO ()
main = do
  config <- execParser parser
  handler <- handler
  let handlers = uncurry singleton handler
  runService config handlers

handler :: IO (ProductDescriptor, Handler Value)
handler = do
    r <- spellChecker
    case r of
      Left err -> print err >> exitFailure
      Right checker -> return (pd, spellcheck checker)
  where pd = ProductDescriptor "text" Errors
