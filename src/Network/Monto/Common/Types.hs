{-# LANGUAGE OverloadedStrings #-}

-- |Types used throughout the Monto protocol.
module Network.Monto.Common.Types
  ( Language(..)
  , NamespacedName(..)
  , Product(..)
  , ProductDescriptor(..)
  , ProductIdentifier(..)
  , ProductName(..)
  , ProtocolVersion(..)
  , SoftwareVersion(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:!)
  , (.=)
  , object
  , pairs
  )
import Data.Aeson.Types (typeMismatch)
import Data.List.Split (splitOn)
import Data.Text
  ( Text
  , intercalate
  , unpack
  )

-- |The language of a product. For example: @haskell@, @python@
--
-- This value is not namespaced.
type Language = Text

-- |A name after a dotted identifier.
data NamespacedName = NamespacedName [Text] Text
  deriving (Eq, Ord, Show)

instance FromJSON NamespacedName where
  parseJSON v@(String s) = do
      let parts = splitOn "." (unpack s)
      if all partOk parts then
        undefined
      else
        typeMismatch "NamespacedName" v
    where charOk c = True
          partOk p = not (null p) && all charOk p

instance ToJSON NamespacedName where
  toJSON (NamespacedName ns n) = String $ intercalate "." (ns ++ [n])

-- |A Product, along with its contents.
data Product = Product ProductIdentifier Value
  deriving (Eq, Show)

instance FromJSON Product where
  parseJSON (Object obj) = construct
      <$> obj .: "language"
      <*> obj .: "name"
      <*> obj .: "path"
      <*> obj .: "contents"
    where construct l n p v = Product (ProductIdentifier l n p) v
  parseJSON v = typeMismatch "Product" v

instance ToJSON Product where
    toJSON (Product (ProductIdentifier language name path) value) = object
      [ "language" .= language
      , "name" .= name
      , "path" .= path
      , "contents" .= value
      ]
    toEncoding (Product (ProductIdentifier language name path) value) = pairs $ mconcat
      [ "language" .= language
      , "name" .= name
      , "path" .= path
      , "contents" .= value
      ]

-- |A Product's name and language.
data ProductDescriptor = ProductDescriptor Language ProductName
  deriving (Eq, Ord, Show)

instance FromJSON ProductDescriptor where
  parseJSON (Object obj) = ProductDescriptor
    <$> obj .: "language"
    <*> obj .: "name"
  parseJSON v = typeMismatch "ProductDescriptor" v

instance ToJSON ProductDescriptor where
    toJSON (ProductDescriptor language name) = object
      [ "language" .= language
      , "name" .= name
      ]
    toEncoding (ProductDescriptor language name) = pairs $ mconcat
      [ "language" .= language
      , "name" .= name
      ]

-- |A Product's name, language, and path.
data ProductIdentifier = ProductIdentifier Language ProductName FilePath
  deriving (Eq, Ord, Show)

instance FromJSON ProductIdentifier where
  parseJSON (Object obj) = ProductIdentifier
    <$> obj .: "language"
    <*> obj .: "name"
    <*> obj .: "path"
  parseJSON v = typeMismatch "ProductIdentifier" v

instance ToJSON ProductIdentifier where
    toJSON (ProductIdentifier language name path) = object
      [ "language" .= language
      , "name" .= name
      , "path" .= path
      ]
    toEncoding (ProductIdentifier language name path) = pairs $ mconcat
      [ "language" .= language
      , "name" .= name
      , "path" .= path
      ]

-- |The name of a Product.
data ProductName
  = Directory
  | Errors
  | Highlighting
  | Source
  | Custom NamespacedName
  deriving (Eq, Ord, Show)

instance FromJSON ProductName where
  parseJSON (String "directory") = return Directory
  parseJSON (String "errors") = return Errors
  parseJSON (String "highlighting") = return Highlighting
  parseJSON (String "source") = return Source
  parseJSON v@(String _) = Custom <$> parseJSON v
  parseJSON v = typeMismatch "ProductName" v

instance ToJSON ProductName where
  toJSON Directory = String "directory"
  toJSON Errors = String "errors"
  toJSON Highlighting = String "highlighting"
  toJSON Source = String "source"
  toJSON (Custom n) = toJSON n

-- |The version number of the Client or Service protocol.
data ProtocolVersion = ProtocolVersion
  { protoMajor :: Int
  , protoMinor :: Int
  , protoPatch :: Int
  } deriving (Eq, Ord, Show)

instance FromJSON ProtocolVersion where
  parseJSON (Object obj) = ProtocolVersion
    <$> obj .: "major"
    <*> obj .: "minor"
    <*> obj .: "patch"
  parseJSON v = typeMismatch "ProtocolVersion" v

instance ToJSON ProtocolVersion where
    toJSON pv = object
      [ "major" .= protoMajor pv
      , "minor" .= protoMinor pv
      , "patch" .= protoPatch pv
      ]
    toEncoding pv = pairs $ mconcat
      [ "major" .= protoMajor pv
      , "minor" .= protoMinor pv
      , "patch" .= protoPatch pv
      ]

-- |The version and implementation of a Client, Broker, or Service.
data SoftwareVersion = SoftwareVersion
  { swId :: String
  , name :: Maybe String
  , vendor :: Maybe String
  , swMajor :: Maybe Int
  , swMinor :: Maybe Int
  , swPatch :: Maybe Int
  } deriving (Eq, Ord, Show)

instance FromJSON SoftwareVersion where
  parseJSON (Object obj) = SoftwareVersion
    <$> obj .: "id"
    <*> obj .:! "name"
    <*> obj .:! "vendor"
    <*> obj .:! "major"
    <*> obj .:! "minor"
    <*> obj .:! "patch"
  parseJSON v = typeMismatch "SoftwareVersion" v

instance ToJSON SoftwareVersion where
    toJSON sv = object
      [ "id" .= swId sv
      , "name" .= name sv
      , "vendor" .= vendor sv
      , "major" .= swMajor sv
      , "minor" .= swMinor sv
      , "patch" .= swPatch sv
      ]
    toEncoding sv = pairs $ mconcat
      [ "id" .= swId sv
      , "name" .= name sv
      , "vendor" .= vendor sv
      , "major" .= swMajor sv
      , "minor" .= swMinor sv
      , "patch" .= swPatch sv
      ]
