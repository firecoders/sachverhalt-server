{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ServerM where

import Control.Monad ((>=>), mzero, void)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- The monad for the server-side implementation sachverhalt
newtype ServerM a = ServerM {
        extractServerM :: ExceptT String (StateT (M.Map T.Text Value) (ReaderT Object IO)) a
    } deriving (Monad, Applicative, Functor, MonadIO)

-- Evaluate a ServerM, usually meaning generating a response to a request
evalServerM :: ServerM a -> Object -> IO (Either String Value)
evalServerM (ServerM m) request = do
    unwrapped <- unwrap
    case unwrapped of
        (Left err, _) -> return . Left $ err
        (_, map) -> return . Right . object . M.toList $ map
    where unwrap = runReaderT (runStateT (runExceptT m) M.empty) request

-- Set a field in the response
setRes :: (ToJSON a) => T.Text -> a -> ServerM ()
setRes key val = ServerM $ lift . modify $ \m -> M.insert key (toJSON val) m

-- Get a field from the response
getRes :: (FromJSON a) => T.Text -> ServerM a
getRes key = do
    v <- parsed
    case v of
        Just v -> return v
        Nothing -> throwServerM "Error: Can't find field in result"
    where parsed = ServerM $ do
            result <- lift . gets $ M.lookup key
            case result of
                Just val -> return . parseMaybe parseJSON $ val
                _ -> return Nothing

-- Modify a field in the response
modifyRes :: (FromJSON a, ToJSON a) => T.Text -> (a -> a) -> ServerM ()
modifyRes key f = do
    val <- getRes key
    setRes key . f $ val

-- Get a field in the request
getReq :: (FromJSON a) => T.Text -> ServerM a
getReq key = do
    obj <- ServerM . lift . lift $ ask
    case parseMaybe (obj .:) key of
        Just v  -> return v
        Nothing -> throwServerM "Error: Can't find field in request"

-- Throw an error
throwServerM :: String -> ServerM a
throwServerM = ServerM . throwError

-- Catch an error
catchServerM :: ServerM a -> (String -> ServerM a) -> ServerM a
catchServerM (ServerM m) f = ServerM . catchError m $ extractServerM . f

-- Run a ServerM and if it fails, return Nothing otherwise return a Just
maybeServerM :: ServerM a -> ServerM (Maybe a)
maybeServerM m = catchServerM (Just `fmap` m) (const . return $ Nothing)

-- Run a ServerM and return wether it succeeds
succeedsServerM :: ServerM a -> ServerM Bool
succeedsServerM m = isJust <$> maybeServerM m
