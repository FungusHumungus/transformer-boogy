{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module MtlStuff where

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.Writer (WriterT(..), MonadWriter(..))

import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock

newtype MyMonadM a = MyMonadM { runMyMonad :: ReaderT String (WriterT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader String, MonadWriter String )

doSomethingAmazing :: ( MonadIO m
                      , MonadTime m
                      , MonadReader String m
                      , MonadWriter String m) => m String
doSomethingAmazing = do
  liftIO $ putStrLn "Lets do something really amazing"
  someStr <- ask
  tell $ "We got a string " ++ someStr

  time <- getCurrentTime

  return $ "This is our string " ++ someStr ++ " at " ++ (show time)
  

class MonadTime m where 
  getCurrentTime :: m UTCTime

instance MonadTime MyMonadM where
  getCurrentTime = liftIO Clock.getCurrentTime
