{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where


import           Control.Applicative       (optional)
import           Control.Monad.Except
import           Data.Default              (def)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, unpack)
import qualified Data.Text.Lazy            as T
import           Network.Browser           (browse, request, setOutHandler,
                                            setProxy)
import           Network.HTTP              (Response(rspBody), getRequest)
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


import           Data.Weather


{------------ Domain ---------------}

type Url = String

data Options =
  Options
  { getConfig :: Config
  , getProxy :: Maybe Proxy
  }

{------------- Main function ----------------}
main :: IO ()
main = cli >>= doSomeWork >>= exitWith


{---------------- Options ------------------}
cli :: IO Options
cli =
  execParser
  $ info (helper <*> opts)
  ( fullDesc
    <> progDesc "Print current weather for CITY"
    <> header "weather - Yahoo Weather displaying tool"
  )


opts :: Parser Options
opts =
  Options
  <$> ( Config
        <$> optional
        ( strOption
          ( long "city"
            <> short 'c'
            <> metavar "CITY"
            <> help "Yahoo weather API's city ID"))

        <*> optional
        ( flag Celsiuses Farenheits
          ( long "farenheits"
            <> short 'F'
            <> help "Show temperature in Farenheits (default: Celsiuses)"
          )
        )
      )

  <*> optional
  ( option extractProxy
    ( long "proxy"
      <> short 'p'
      <> help "Proxy server in format [user:pass@]host[:port]"
    )
  )

  where
    extractProxy =
      readerAsk
      >>= maybe (readerError "Wrong proxy string! (see --help)") return
      . parseProxy


{------------------ Misc stuff --------------------------}

renderWeather :: Weather -> String
renderWeather w =
  concat
  [ unpack (getCity w)
  , unpack (getTemp w)
  , case getUnits w of
    Celsiuses  -> "°C"
    Farenheits -> "°F"
  , ", "
  , unpack (getText w)
  ]


mkAPIUrl :: CityID -> TempUnits -> Url
mkAPIUrl city units =
  let
    unitStr = case units of
      Celsiuses  -> "c"
      Farenheits -> "f"
  in "http://weather.yahooapis.com/forecastrss?w=" ++ city ++ "&u=" ++ unitStr


getWeather :: Monad m => Document -> ExceptT String m Weather
getWeather doc =
  let
    weather = do
      feed      <- doc ^? root . el "rss" ./ el "channel"
      units     <- feed ^? el "channel" ./ named "units" . attr "temperature"
      city      <- feed ^? el "channel" ./ named "location" . attr "city"
      condition <- feed ^? el "channel" ./ el "item" ./ named "condition"

      Weather city
        <$> condition ^? attr "temp"
        <*> toTempUnit units
        <*> condition ^? attr "text"
  in
    maybe (fail "Bad response!") return weather
  where
    toTempUnit :: Text -> Maybe TempUnits
    toTempUnit = flip lookup [("C", Celsiuses), ("F", Farenheits)]


simpleRequest :: Maybe Proxy -> Url -> ExceptT String IO String
simpleRequest mbProxy url =
  liftIO get `catchError` (fail . show)
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


parseDocument :: Monad m => String -> ExceptT String m Document
parseDocument = try . X.parseText def . T.pack
  where
    try = either (fail . show) return


doSomeWork :: Options -> IO ExitCode
doSomeWork o =
  let
    cfg = getConfig o
    req = simpleRequest
          (getProxy o)
          ( mkAPIUrl ( fromMaybe
                       "2121267" -- default city is Kazan'
                       (cityID cfg)
                     )
            (fromMaybe Celsiuses (tempUnits cfg))
          )
  in
    runExceptT (req >>= parseDocument >>= getWeather) >>= \case
      Left err -> do
        putStrLn err
        return $ ExitFailure 1

      Right w -> do
        putStrLn . renderWeather $ w
        return ExitSuccess
