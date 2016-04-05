{-# LANGUAGE OverloadedStrings #-}

module Network.Weather.Provider.OWM
  ( owm
  ) where

import           Control.Monad.Except      (ExceptT)
import           Data.Default              (def)
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as T
import           Text.XML                  (Document)
import qualified Text.XML                  as X
import           Text.XML.Lens             (attr, el, named, root, (./), (^?))

import           Data.Weather
import           Network.Weather.Provider
import           Network.Weather.Utils

owm :: Provider
owm =
  Provider
  { getName = "owm"
  , getDescription = "Open Weather Map"
  , getWeather = getWeather'
  }


getWeather' :: (Url -> Response) -> CityID -> TempUnits -> Result
getWeather' req c t = req (mkAPIUrl c t) >>= parseDocument >>= extractWeather


mkAPIUrl :: CityID -> TempUnits -> Url
mkAPIUrl city units =
  let
    unitStr = case units of
      Celsiuses  -> "c"
      Farenheits -> "f"
  in "http://weather.yahooapis.com/forecastrss?w=" ++ city ++ "&u=" ++ unitStr


extractWeather :: Monad m => Document -> ExceptT String m Weather
extractWeather doc =
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


parseDocument :: Monad m => String -> ExceptT String m Document
parseDocument = try . X.parseText def . T.pack
  where
    try = either (fail . show) return
