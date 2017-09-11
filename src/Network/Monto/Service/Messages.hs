{-# LANGUAGE OverloadedStrings #-}

module Network.Monto.Service.Messages
  ( BrokerRequest(..)
  , ServiceBrokerNegotiation(..)
  , ServiceErrors(..)
  , ServiceNegotiation(..)
  , ServiceProduct(..)
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
import Data.Aeson.Types (typeMismatch)
import Data.Set (Set)
import qualified Data.Set as Set
import Network.Monto.Common.Types
  ( NamespacedName
  , Product
  , ProductDescriptor
  , ProductIdentifier
  , ProtocolVersion
  , SoftwareVersion
  )
import Network.Monto.Service.Types
  ( ServiceError(..)
  , ServiceNotice(..)
  )

data BrokerRequest = BrokerRequest
  { request :: ProductIdentifier
  , products :: [Product]
  } deriving (Eq, Show)

instance FromJSON BrokerRequest where
  parseJSON (Object obj) = BrokerRequest
    <$> obj .: "request"
    <*> obj .: "products"
  parseJSON v = typeMismatch "BrokerRequest" v

data ServiceBrokerNegotiation = ServiceBrokerNegotiation
  { sbnMontoVersion :: ProtocolVersion
  , brokerVersion :: SoftwareVersion
  , sbnExtensions :: Set NamespacedName
  } deriving (Eq, Ord, Show)

instance FromJSON ServiceBrokerNegotiation where
  parseJSON (Object obj) = ServiceBrokerNegotiation
    <$> obj .: "monto"
    <*> obj .: "broker"
    <*> obj .:! "extensions" .!= Set.empty
  parseJSON v = typeMismatch "ServiceBrokerNegotiation" v

data ServiceErrors = ServiceErrors
  { errors :: [ServiceError]
  , seNotices :: [ServiceNotice]
  } deriving (Eq, Ord, Show)

instance ToJSON ServiceErrors where
  toJSON se = object
    [ "errors" .= errors se
    , "notices" .= seNotices se
    ]
  toEncoding se = pairs $ mconcat
    [ "errors" .= errors se
    , "notices" .= seNotices se
    ]

data ServiceNegotiation = ServiceNegotiation
  { snMontoVersion :: ProtocolVersion
  , serviceVersion :: SoftwareVersion
  , snExtensions :: Set NamespacedName
  , snProducts :: Set ProductDescriptor
  } deriving (Eq, Ord, Show)

instance FromJSON ServiceNegotiation where
  parseJSON (Object obj) = ServiceNegotiation
    <$> obj .: "monto"
    <*> obj .: "service"
    <*> obj .:! "extensions" .!= Set.empty
    <*> obj .: "products"
  parseJSON v = typeMismatch "ServiceNegotiation" v

instance ToJSON ServiceNegotiation where
    toJSON sn = object
      [ "monto" .= snMontoVersion sn
      , "service" .= serviceVersion sn
      , "extensions" .= snExtensions sn
      , "products" .= snProducts sn
      ]
    toEncoding sn = pairs $ mconcat
      [ "monto" .= snMontoVersion sn
      , "service" .= serviceVersion sn
      , "extensions" .= snExtensions sn
      , "products" .= snProducts sn
      ]

data ServiceProduct = ServiceProduct
  { spProduct :: Product
  , spNotices :: [ServiceNotice]
  } deriving (Eq, Show)

instance ToJSON ServiceProduct where
  toJSON sp = object
    [ "product" .= spProduct sp
    , "notices" .= spNotices sp
    ]
  toEncoding sp = pairs $ mconcat
    [ "product" .= spProduct sp
    , "notices" .= spNotices sp
    ]
