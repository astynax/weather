{-# LANGUAGE OverloadedStrings #-}

module Network.Weather.Provider.OWM
  ( owm
  ) where

import           Network.Weather.Provider

owm :: Provider
owm =
  Provider
  { getName = "owm"
  , getDescription = "Open Weather Map"
  , getWeather = \_ _ _ -> fail "Not implemented!"
  }
