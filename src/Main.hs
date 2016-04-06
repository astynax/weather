{-# LANGUAGE LambdaCase #-}

module Main where


import           Control.Applicative       (optional)
import           Control.Monad.Except
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (unpack)
import           Network.HTTP.Proxy        (Proxy, parseProxy)
import           Options.Applicative       (Parser, execParser, flag, fullDesc,
                                            header, help, helper, info, long,
                                            metavar, option, progDesc, short,
                                            strOption, (<>))
import           Options.Applicative.Types (readerAsk, readerError)
import           System.Exit               (ExitCode(..), exitWith)

import           Data.Weather
import           Network.Weather.Provider
import           Network.Weather.Provider.OWM
import           Network.Weather.Utils


data Options =
  Options
  { getCityID :: Maybe CityID
  , getTempUnits :: Maybe TempUnits
  , getProxy :: Maybe Proxy
  }


main :: IO ()
main = configure >>= run >>= exitWith


configure :: IO Options
configure =
  execParser
  $ info (helper <*> opts)
  ( fullDesc
    <> progDesc "Print current weather for CITY"
    <> header "weather - Yahoo Weather displaying tool"
  )


opts :: Parser Options
opts =
  Options
  <$> optional city
  <*> optional tempUnits
  <*> optional proxy
  where
    city =
      strOption
      ( long "city"
        <> short 'c'
        <> metavar "CITY"
        <> help "Yahoo weather API's city ID"
      )

    tempUnits =
      flag Celsiuses Farenheits
      ( long "farenheits"
        <> short 'F'
        <> help "Show temperature in Farenheits (default: Celsiuses)"
      )

    proxy =
      option extractProxy
      ( long "proxy"
        <> short 'p'
        <> help "Proxy server in format [user:pass@]host[:port]"
      )

    extractProxy =
      readerAsk
      >>= maybe (readerError "Wrong proxy string! (see --help)") return
      . parseProxy


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


run :: Options -> IO ExitCode
run o =
  let
    cid = fromMaybe "2121267" $ getCityID o  -- default city is Kazan'
    tu = fromMaybe Celsiuses $ getTempUnits o
    req = request $ getProxy o
  in
    runExceptT (getWeather owm req cid tu) >>= \case
      Left err -> do
        putStrLn err
        return $ ExitFailure 1

      Right w -> do
        putStrLn . renderWeather $ w
        return ExitSuccess
