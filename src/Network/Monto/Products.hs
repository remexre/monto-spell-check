{-# LANGUAGE OverloadedStrings #-}

module Network.Monto.Products
  ( Directory(..)
  , DirectoryEntry(..)
  , DirectoryEntryType(..)
  , Error(..)
  , ErrorSeverity(..)
  , Source(..)
  ) where

import Data.Aeson.Types
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.=)
  , object
  , typeMismatch
  )
import Data.Text (Text)

type Directory = [DirectoryEntry]

data DirectoryEntry = DirectoryEntry String FilePath DirectoryEntryType
  deriving (Eq, Ord, Show)

instance FromJSON DirectoryEntry where
  parseJSON (Object obj) = DirectoryEntry
    <$> obj .: "name"
    <*> obj .: "absolute_path"
    <*> obj .: "type"
  parseJSON v = typeMismatch "DirectoryEntry" v

data DirectoryEntryType
  = File
  | Directory
  | Symlink
  | Other
  deriving (Eq, Ord, Show)

instance FromJSON DirectoryEntryType where
  parseJSON (String "file") = return File
  parseJSON (String "directory") = return Directory
  parseJSON (String "symlink") = return Symlink
  parseJSON (String "other") = return Other
  parseJSON v = typeMismatch "DirectoryEntryType" v

data Error = Error
  { message :: Text
  , startByte :: Int
  , endByte :: Int
  , severity :: Maybe ErrorSeverity
  } deriving (Eq, Ord, Show)

instance ToJSON Error where
  toJSON err = object $ addSeverity
      [ "message" .= message err
      , "startByte" .= startByte err
      , "endByte" .= endByte err
      ]
    where addSeverity = case severity err of
            Just s -> (:) ("severity" .= s)
            Nothing -> id

data ErrorSeverity
  = SeverityError
  | SeverityWarning
  | SeverityInfo
  deriving (Eq, Ord, Show)

instance ToJSON ErrorSeverity where
  toJSON SeverityError = String "error"
  toJSON SeverityWarning = String "warning"
  toJSON SeverityInfo = String "info"

type Source = Text
