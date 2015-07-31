{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Applicative       (optional, (<$>), (<*>))
import           Control.Monad             (liftM)
import           Data.Default              (def)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, unpack)
import qualified Data.Text.Lazy            as T
import           Network.Browser           (browse, request, setOutHandler,
                                            setProxy)
import           Network.HTTP              (Response (rspBody), catchIO,
                                            getRequest)
import           Network.HTTP.Proxy        (Proxy, fetchProxy, parseProxy)
import           Options.Applicative       (Parser, execParser, flag, fullDesc,
                                            header, help, helper, info, long,
                                            metavar, option, progDesc, short,
                                            strOption, (<>))
import           Options.Applicative.Types (readerAsk, readerError)
import           System.Exit               (ExitCode(..), exitWith)
import           Text.XML                  (Document)
import qualified Text.XML                  as X
import           Text.XML.Lens             (attr, el, named, root, (./), (^?))


{------------ Domain ---------------}
type CityID = String

data TempUnits = Celsiuses
               | Farenheits

data Weather = Weather
             { getCity    :: Text
             , getCountry :: Text
             , getTemp    :: Text
             , getUnits   :: TempUnits
             , getDate    :: Text
             , getText    :: Text }

data Config = Config
            { cityID    :: Maybe CityID
            , tempUnits :: TempUnits
            , proxy     :: Maybe Proxy }

type Url = String


{------------- Main function ----------------}
main :: IO ()
main = cli >>= doSomeWork >>= exitWith


{---------------- Options ------------------}
cli :: IO Config
cli = execParser
    $ info (helper <*> opts)
      (fullDesc
    <> progDesc "Print current weather for CITY"
    <> header "weather - Yahoo Weather displaying tool")


opts :: Parser Config
opts = Config
  <$> optional (strOption
      (long "city"
    <> short 'c'
    <> metavar "CITY"
    <> help "Yahoo weather API's city ID"))

  <*> flag Celsiuses Farenheits
      (long "farenheits"
    <> short 'F'
    <> help "Show temperature in Farenheits (default: Celsiuses)")

  <*> optional (option extractProxy
      (long "proxy"
    <> short 'p'
    <> help "Proxy server in format [user:pass@]host[:port]"))

  where
    extractProxy = readerAsk
               >>= maybe (readerError "Wrong proxy string! (see --help)") return
                 . parseProxy


{------------------ Misc stuff --------------------------}

renderWeather :: Weather -> String
renderWeather w =
  concat [ unpack (getDate w), ": "
         , unpack (getCity w)
         , "(", unpack (getCountry w), "), "
         , unpack (getTemp w)
         , case getUnits w of
              Celsiuses  -> "°C"
              Farenheits -> "°F"
         , ", "
         , unpack (getText w)]


mkAPIUrl :: CityID -> TempUnits -> Url
mkAPIUrl city units =
  let unitStr = case units of
                  Celsiuses  -> "c"
                  Farenheits -> "f"
  in "http://weather.yahooapis.com/forecastrss?w=" ++ city ++ "&u=" ++ unitStr


getWeather :: Document -> Maybe Weather
getWeather doc =
  do feed      <- doc ^? root . el "rss" ./ el "channel"
     units     <- feed ^? el "channel" ./ named "units" . attr "temperature"
     city      <- feed ^? el "channel" ./ named "location" . attr "city"
     country   <- feed ^? el "channel" ./ named "location" . attr "country"
     condition <- feed ^? el "channel" ./ el "item" ./ named "condition"
     Weather city country
         <$> condition ^? attr "temp"
         <*> toTempUnit units
         <*> condition ^? attr "date"
         <*> condition ^? attr "text"
  where
    toTempUnit :: Text -> Maybe TempUnits
    toTempUnit = flip lookup [("C", Celsiuses), ("F", Farenheits)]


simpleRequest :: Maybe Proxy -> Url -> IO (Maybe String)
simpleRequest mbProxy url =
  catchIO (liftM Just get)
          (const (return Nothing))
  where
    get :: IO String
    get = do
      -- if no proxy specified, trying to fetch from OS
      p <- maybe (fetchProxy False) return mbProxy
      (_, res) <- browse $ do
        setProxy p
        setOutHandler $ const $ return ()
        request (getRequest url)
      return (rspBody res)


parseDocument :: String -> Maybe Document
parseDocument s =
  case X.parseText def (T.pack s) of
    Right d -> Just d
    _       -> Nothing


doSomeWork :: Config -> IO ExitCode
doSomeWork cfg = do
  resp <- simpleRequest
    (proxy cfg)
    (mkAPIUrl (fromMaybe "2121267" -- default city is Kazan'
                         (cityID cfg))
              (tempUnits cfg))
  let weather = resp >>= parseDocument >>= getWeather
  maybe (return $ ExitFailure 1)
        ((>> return ExitSuccess) . putStrLn . renderWeather)
        weather
