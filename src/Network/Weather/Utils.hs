module Network.Weather.Utils
  ( Url, NetworkError, Response
  , request
  ) where

import           Control.Monad.Except
import           Network.Browser           (browse, setOutHandler, setProxy)
import qualified Network.Browser           as B
import           Network.HTTP              (getRequest)
import qualified Network.HTTP              as HTTP
import           Network.HTTP.Proxy        (Proxy, fetchProxy)


type Url = String
type NetworkError = String
type Response = ExceptT NetworkError IO String


request :: Maybe Proxy -> Url -> Response
request mbProxy url =
  liftIO get `catchError` (fail . show)
  where
    get :: IO String
    get = do
      -- if no proxy specified, trying to fetch from OS
      p <- maybe (fetchProxy False) return mbProxy
      (_, res) <- browse $ do
        setProxy p
        setOutHandler $ const $ return ()
        B.request (getRequest url)
      return (HTTP.rspBody res)
