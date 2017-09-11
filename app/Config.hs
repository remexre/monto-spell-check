{-# LANGUAGE OverloadedStrings #-}

module Config
  ( parser
  ) where

import Control.Applicative ((<**>))
import Data.Monoid ((<>))
import Network.Monto.Common.Types (SoftwareVersion(..))
import Network.Monto.Service (Config(..))
import Options.Applicative
  ( Parser
  , ParserInfo
  , auto
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , str
  , strOption
  , value
  )

parser :: ParserInfo Config
parser = info (configParser <**> helper)
  ( fullDesc
 <> progDesc "A spell-checking Monto service" )

configParser :: Parser Config
configParser = Config
  <$> pure version
  <*> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "The port to serve on"
     <> showDefault
     <> value 28888 )
  <*> option str
       ( long "serve-on"
      <> metavar "HOST-PREFERENCE"
      <> help "The host to serve on" 
      <> showDefault
      <> value "*" )

version :: SoftwareVersion
version = SoftwareVersion
  { swId = "edu.umn.cs.melt.spellcheck"
  , name = Just "Spell Check"
  , vendor = Just "MELT"
  , swMajor = Just 0
  , swMinor = Just 0
  , swPatch = Just 0
  }
