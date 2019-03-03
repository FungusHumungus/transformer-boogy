module Lib
    ( someFunc
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Control.Monad.Trans.Reader (ReaderT(..), runReaderT, ask)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT, tell)


hoistMaybe :: Monad m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

sortofmultiplyandprint :: Maybe Integer -> Maybe Integer -> MaybeT IO String
sortofmultiplyandprint a b = do
  a' <- hoistMaybe a
  b' <- hoistMaybe b
  lift $ putStrLn $ "Got a " <> show a'
  lift $ putStrLn $ "Got a " <> show b'
  return $ show ( a' * b' )
  


someFunc :: IO ()
someFunc = putStrLn "someFunc"

sortofmultiply :: Maybe Int -> Maybe Int -> Maybe String
sortofmultiply a b = do
  a' <- a
  b' <- b
  return $ show ( a' * b' )


doSomethingAmazing :: Maybe String -> MaybeT (ReaderT String (WriterT String IO)) String
doSomethingAmazing str = do
  (lift . lift . lift) $ putStrLn "Lets do something amazing"
  str' <- hoistMaybe str
  anotherStr <- lift ask
  (lift . lift) $ tell $ "We got some strings " ++ str' ++ " and " ++ anotherStr

  return $ "These are our strings " ++ str' ++ ", " ++ anotherStr
  
