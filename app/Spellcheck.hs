{-# LANGUAGE OverloadedStrings #-}

module Spellcheck
  ( spellcheck
  , spellcheckText
  ) where

import Control.Arrow ((>>>))
import Data.Aeson
  ( ToJSON(..)
  , Value(..)
  )
import qualified Data.ByteString.Char8 as B
import Data.Char
  ( chr
  , isSpace
  )
import Data.List.Split (splitWhen)
import Data.Text
  ( Text
  , pack
  , unpack
  )
import Language.Aspell
  ( SpellChecker
  , check
  )
import Network.Monto.Common.Types
  ( ProductIdentifier(..)
  , ProductName(..)
  )
import Network.Monto.Service
  ( Handler
  , readSource
  , request
  )
import Network.Monto.Products
  ( Error(..)
  , ErrorSeverity(..)
  )

spellcheck :: SpellChecker -> Handler Value
spellcheck checker = do
  ProductIdentifier "text" Errors path <- request
  src <- readSource "text" path
  let errs = spellcheckText checker src
  return $ toJSON errs

spellcheckText :: SpellChecker -> Text -> [Error]
spellcheckText checker = unpack
                     >>> addLocations
                     >>> splitWhen (isSplitChar . fst)
                     >>> filter (not . null)
                     >>> filter (not . check checker . B.pack . map fst)
                     >>> map asError

addLocations :: [Char] -> [(Char, Int)]
addLocations cs = helper 0 cs
  where helper _ [] = []
        helper acc (h:t) = (h, acc) : helper (acc + sizeof h) t
        sizeof c = if c <= chr 0x7f then 1
                   else if c <= chr 0x7ff then 2
                   else if c <= chr 0xffff then 3
                   else if c <= chr 0x10ffff then 4
                   else error "invalid char"

asError :: [(Char, Int)] -> Error
asError cs = Error
  { message = mconcat
    [ "The word '"
    , pack $ map fst cs
    , "' may be misspelled."
    ]
  , startByte = snd $ head cs
  , endByte = snd $ last cs
  , severity = Just SeverityError
  }

isSplitChar :: Char -> Bool
isSplitChar c = isSpace c
