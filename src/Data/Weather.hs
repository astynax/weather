module Data.Weather
  ( TempUnits(..)
  , Weather(..)
  , Config(..)
  , CityID
  , apply, withConfig
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)

type CityID = String

data TempUnits = Celsiuses | Farenheits

data Weather =
  Weather
  { getCity :: Text
  , getTemp :: Text
  , getUnits :: TempUnits
  , getText :: Text
  }

data Config =
  Config
  { cityID :: Maybe CityID
  , tempUnits :: Maybe TempUnits
  }

type WeatherResult = IO (Either String Weather)

type Provider = CityID -> TempUnits -> WeatherResult


apply :: Config -> Config -> Config
apply (Config c1 u1) (Config c2 u2) = Config (c1 <|> c2) (u1 <|> u2)


withConfig :: Config -> Provider -> WeatherResult
withConfig cfg prov =
  case cfg of
    (Config Nothing _) -> err "No cityID specified!"
    (Config _ Nothing) -> err "No TempUnits specified!"
    (Config (Just c) (Just u)) -> prov c u
  where
    err = return . Left
