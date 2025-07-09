{-# LANGUAGE OverloadedStrings #-}

module AkinfProject.Filter (filterByConfig) where

import AkinfProject.Config (Config(..), Timeframe(..))
import AkinfProject.CSV (Stock(..))
import Data.Time (parseTimeM, defaultTimeLocale, Day)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- Normalize stock names to uppercase and trim whitespace
normalize :: String -> String
normalize = T.unpack . T.strip . T.toUpper . T.pack

-- Parse date in "YYYY-MM-DD" format
parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Filter the stock data by config's stock list and date range
filterByConfig :: Config -> V.Vector Stock -> Map.Map String (V.Vector Stock)
filterByConfig (Config stockNames (Timeframe start end)) allStocks =
  let startDay = parseDay start
      endDay = parseDay end
      normalizedNames = map normalize stockNames

      inRange stock = case parseDay (date stock) of
        Just d -> case (startDay, endDay) of
                    (Just s, Just e) -> s <= d && d <= e
                    _ -> False
        _ -> False

      wanted stock = normalize (name stock) `elem` normalizedNames && inRange stock

      grouped = V.foldr
        (\s acc -> if wanted s
                    then Map.insertWith (V.++) (name s) (V.singleton s) acc
                    else acc)
        Map.empty
        allStocks
  in grouped
