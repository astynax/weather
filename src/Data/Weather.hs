module Data.Weather
  ( TempUnits(..)
  , Weather(..)
  ) where

import Data.Text (Text)

data TempUnits = Celsiuses | Farenheits

data Weather =
  Weather
  { getCity :: Text
  , getTemp :: Text
  , getUnits :: TempUnits
  , getText :: Text
  }
