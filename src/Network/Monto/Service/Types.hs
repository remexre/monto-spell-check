{-# LANGUAGE OverloadedStrings #-}

module Network.Monto.Service.Types
  ( ServiceError(..)
  , ServiceNotice(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.!=)
  , (.:)
  , (.:!)
  , (.=)
  , object
  , pairs
  )
import Data.Text (Text)
import Network.Monto.Common.Types (ProductIdentifier)

data ServiceError
  = UnmetDependency ProductIdentifier
  | ProtocolViolation Text
  | OtherError Text
  deriving (Eq, Ord, Show)

seToTV :: ServiceError -> (Text, Value)
seToTV (UnmetDependency pi) = ("unmet_dependency", toJSON pi)
seToTV (ProtocolViolation s) = ("other", String s)
seToTV (OtherError s) = ("other", String s)

instance ToJSON ServiceError where
  toJSON se = object
      [ "type" .= t
      , "value" .= v
      ]
    where (t, v) = seToTV se
  toEncoding se = pairs $ mconcat
      [ "type" .= t
      , "value" .= v
      ]
    where (t, v) = seToTV se

data ServiceNotice
  = UnusedDependency ProductIdentifier
  deriving (Eq, Ord, Show)

instance ToJSON ServiceNotice where
  toJSON (UnusedDependency pi) = object
    [ "type" .= String "unused_dependency"
    , "value" .= pi
    ]
  toEncoding (UnusedDependency pi) = pairs $ mconcat
    [ "type" .= String "unused_dependency"
    , "value" .= pi
    ]
