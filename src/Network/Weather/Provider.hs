module Network.Weather.Provider
  ( Provider(..), CityID, ProviderError, Result
  ) where

import           Control.Monad.Except (ExceptT)

import           Data.Weather (Weather, TempUnits)
import           Network.Weather.Utils

data Provider =
  Provider
  { getName :: String
  , getDescription :: String
  , getWeather :: (Url -> Response) -> CityID -> TempUnits -> Result
  }

type ProviderError = String
type CityID = String
type Result = ExceptT ProviderError IO Weather
