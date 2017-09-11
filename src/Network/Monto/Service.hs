{-# LANGUAGE OverloadedStrings #-}

module Network.Monto.Service
  ( Config(..)
  , Handler
  , fatalError
  , getProduct
  , nonfatalError
  , notice
  , readDir
  , readSource
  , request
  , runService
  , serviceApp
  ) where

import Data.Aeson
  ( FromJSON(..)
  , Value
  , eitherDecodeStrict
  , encode
  )
import qualified Data.Binary.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
  ( byteString
  , stringUtf8
  , toLazyByteString
  )
import Data.ByteString.Lazy (toStrict)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Network.HTTP.Types
  ( StdMethod(..)
  , parseMethod
  )
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( badRequest400
  , conflict409
  , internalServerError500
  , notFound404 
  , ok200
  , serviceUnavailable503
  )
import Network.Monto.Common.Types
  ( Product(..)
  , ProductDescriptor(..)
  , ProductIdentifier(..)
  , ProtocolVersion(..)
  , SoftwareVersion(..)
  )
import Network.Monto.Service.Handler
  ( Handler
  , fatalError
  , getProduct
  , nonfatalError
  , notice
  , readDir
  , readSource
  , request
  , runHandlerFull
  )
import Network.Monto.Service.Messages
  ( BrokerRequest(BrokerRequest)
  , ServiceBrokerNegotiation(..)
  , ServiceErrors(..)
  , ServiceNegotiation(..)
  , ServiceProduct(..)
  )
import Network.Wai
  ( Application
  , Request
  , pathInfo
  , requestBody
  , requestHeaders
  , requestMethod
  , responseBuilder
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( HostPreference
  , Port
  , defaultSettings
  , runSettings
  , setHost
  , setPort
  , setServerName
  )

data Config = Config
  { service :: SoftwareVersion
  , port :: Port
  , serveOn :: HostPreference
  } deriving (Eq, Show)

runService :: Config -> Map ProductDescriptor (Handler Value) -> IO ()
runService config handlers = runSettings settings $ serviceApp (service config) handlers
  where setName = case name $ service config of
                     Just name -> setServerName (toStrict $ toLazyByteString $ stringUtf8 $ name)
                     Nothing -> id
        settings = setHost (serveOn config)
                 $ setName
                 $ setPort (port config)
                 $ defaultSettings

body :: FromJSON a => Request -> IO (Either String a)
body req = helper BS.empty
  where helper acc = do
          bs <- requestBody req
          if bs == BS.empty then
            return $ eitherDecodeStrict acc
          else
            helper (acc <> bs)

serviceApp :: SoftwareVersion -> Map ProductDescriptor (Handler Value) -> Application
serviceApp sv handlers req respond = case (parseMethod $ requestMethod req, pathInfo req) of
  (Right POST, ["monto", "service"]) -> do
    let headers = requestHeaders req
    let extensions = map snd $ filter ((==) "" . fst) headers
    if not $ null extensions then do
      putStrLn $ "Protocol violation: Using unsupported extensions " ++ show extensions
      respond $ responseBuilder serviceUnavailable503 [] Builder.empty
    else do
      r <- body req
      case r of
        Left err -> do
          print err
          respond $ responseBuilder serviceUnavailable503 [] Builder.empty
        Right (BrokerRequest pi products) -> do
          let pd = let ProductIdentifier language productName _ = pi
                   in ProductDescriptor language productName
          (status, body) <- case Map.lookup pd handlers of
              Just handler -> do
                (r, notices) <- runHandlerFull handler pi products
                return $ case r of
                  Left errs -> (internalServerError500, encode $ ServiceErrors errs notices)
                  Right val -> (ok200, encode $ ServiceProduct (Product pi val) notices)
              Nothing -> return (badRequest400, encode pi)
          let headers =
                [ (hContentType, "application/json")
                ]
          respond $ responseLBS status headers body
  (Right POST, ["monto", "version"]) -> do
    sbn <- body req
    status <- case sbn of
      Left err -> print err >> return serviceUnavailable503
      Right (ServiceBrokerNegotiation { sbnMontoVersion = mv }) -> return $
        if protoMajor mv /= 3
        then conflict409
        else ok200
    let headers =
          [ (hContentType, "application/json")
          ]
    let sn = ServiceNegotiation
              { snMontoVersion = ProtocolVersion
                { protoMajor = 3
                , protoMinor = 0
                , protoPatch = 0
                }
              , serviceVersion = sv
              , snExtensions = Set.empty
              , snProducts = Map.keysSet handlers
              }
    respond $ responseLBS status headers (encode sn)
  _ -> respond $ responseBuilder notFound404 [] Builder.empty
