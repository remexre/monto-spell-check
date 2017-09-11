{-# LANGUAGE OverloadedStrings #-}

-- |The 'Handler' monad, used for control flow in handlers.
module Network.Monto.Service.Handler
  ( Handler
  , fatalError
  , getProduct
  , nonfatalError
  , notice
  , readDir
  , readSource
  , request
  , runHandlerFull
  ) where

import Control.Monad
  ( ap
  , liftM
  )
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson.Types
  ( FromJSON(..)
  , Value(String)
  , parseEither
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
  ( Text
  , pack
  )
import Network.Monto.Common.Types
  ( Language
  , Product(..)
  , ProductIdentifier(..)
  , ProductName(..)
  )
import Network.Monto.Products
  ( Directory
  , Source
  )
import Network.Monto.Service.Types
  ( ServiceError(..)
  , ServiceNotice(..)
  )

-- |A monad for the responses to a product request.
newtype Handler a = Handler
  { runHandler :: ProductIdentifier -> Map ProductIdentifier (Value, Bool) -> IO (Maybe a, Set ServiceError, Set ServiceNotice, Map ProductIdentifier (Value, Bool))
  }

instance Functor Handler where
  fmap = liftM

instance Applicative Handler where
  pure x = Handler $ \_ ps -> return (Just x, Set.empty, Set.empty, ps)
  (<*>) = ap

instance Monad Handler where
  (>>=) (Handler h) f = Handler $ \pi ps -> do
    (x, es, ns, ps') <- h pi ps
    case x of
      Just val -> do
        let Handler h' = f val
        (y, es', ns', ps'') <- h' pi ps
        return (y, es <> es', ns <> ns', ps'')
      Nothing -> return (Nothing, es, ns, ps')

instance MonadIO Handler where
  liftIO m = Handler $ \_ ps -> do
    x <- m
    return (Just x, Set.empty, Set.empty, ps)

-- |Adds an error to the response.
--
-- Will /not/ terminate the Handler monad; if you want to do so, use
-- 'fatalError' instead.
nonfatalError :: ServiceError -> Handler ()
nonfatalError err = Handler $ \_ ps -> return (Just (), Set.singleton err, Set.empty, ps)

-- |Terminates the Handler monad with an error.
fatalError :: ServiceError -> Handler a
fatalError err = Handler $ \_ ps -> return (Nothing, Set.singleton err, Set.empty, ps)

-- |Reads a product from the client.
getProduct :: FromJSON a => ProductIdentifier -> Handler a
getProduct pi = tryGetProduct pi >>= helper
  where helper Nothing = fatalError $ UnmetDependency pi
        helper (Just p) = return p

-- |Adds a notice to the response.
--
-- Note that 'UnusedDependency' notices will be sent automatically for
-- successful responses.
notice :: ServiceNotice -> Handler ()
notice notice = Handler $ \_ ps -> return (Just (), Set.empty, Set.singleton notice, ps)

-- |Reads a directory (a @directory@ product) with the given path.
readDir :: FilePath -> Handler Directory
readDir path = getProduct $ ProductIdentifier "none" Directory path

-- |Reads a file (a @source@ product) of the given language and path.
readSource :: Language -> FilePath -> Handler Source
readSource language path = getProduct $ ProductIdentifier language Source path

-- |Returns the 'ProductIdentifier' requested.
request :: Handler ProductIdentifier
request = Handler $ \pi ps -> return (Just pi, Set.empty, Set.empty, ps)

-- |Runs a handler, converting any unused products to 'UnusedDependency's.
runHandlerFull :: Handler a -> ProductIdentifier -> [Product] -> IO (Either [ServiceError] a, [ServiceNotice])
runHandlerFull handler req products = do
  -- runHandler :: ProductIdentifier -> Map ProductIdentifier (Value, Bool) -> IO (Maybe a, Set ServiceError, Set ServiceNotice, Map ProductIdentifier (Value, Bool))
    let products' = Map.fromList $ map transformProduct products
    (r, errs, notices, products'') <- runHandler handler req products'
    let notices' = Set.toList notices <> (catMaybes $ map unused $ Map.toList products'')
    let r' = case r of
          Just x -> Right x
          Nothing -> Left $ Set.toList errs
    return (r', notices')
  where transformProduct (Product id val) = (id, (val, False))
        unused (pi, (_, False)) = Just $ UnusedDependency pi
        unused _ = Nothing

-- |Tries to read a product from the client. If the product does not exist,
-- @Nothing@ will be returned. If the product cannot be decoded into the
-- output type, a fatal error occurs.
tryGetProduct :: FromJSON a => ProductIdentifier -> Handler (Maybe a)
tryGetProduct pi@(ProductIdentifier language name path) = Handler $ \_ ps -> do
  let (val, ps') = case Map.lookup pi ps of
       Just (val, _) -> (Just val, Map.insert pi (val, True) ps)
       Nothing -> (Nothing, ps)
  let (val', errs) = case val of
       Just val -> case parseEither parseJSON val of
         Left err -> (Nothing, Set.singleton $ OtherError $ pack err)
         Right x -> (Just $ Just x, Set.empty)
       Nothing -> (Just Nothing, Set.empty)
  return (val', errs, Set.empty, ps')
