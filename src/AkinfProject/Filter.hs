{-# LANGUAGE OverloadedStrings #-}

module AkinfProject.Filter (filterByConfig) where

import AkinfProject.Config (Config(..), Timeframe(..))
import AkinfProject.CSV (Stock(..))
import Data.Time (parseTimeM, defaultTimeLocale, Day)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- Normalize stock names to uppercase and trim whitespace (now works with Text)
normalize :: T.Text -> T.Text
normalize = T.strip . T.toUpper

-- Helper function to convert String config values to Text for comparison
normalizeString :: String -> T.Text
normalizeString = T.strip . T.toUpper . T.pack

-- Parse date in "YYYY-MM-DD" format (works with Text)
parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Parse Text date into Day
parseDayText :: T.Text -> Maybe Day
parseDayText = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

-- Filter the stock data by config's stock list and date range
filterByConfig :: Config -> V.Vector Stock -> Map.Map String (V.Vector Stock)
filterByConfig (Config stockNames (Timeframe start end)) allStocks =
  let startDay = parseDay start
      endDay = parseDay end
      normalizedNames = map normalizeString stockNames

      inRange stock = case parseDayText (date stock) of
        Just d -> case (startDay, endDay) of
                    (Just s, Just e) -> s <= d && d <= e
                    _ -> False
        _ -> False

      wanted stock = normalize (name stock) `elem` normalizedNames && inRange stock

      grouped = V.foldr
        (\s acc -> if wanted s
                    then Map.insertWith (V.++) (T.unpack (name s)) (V.singleton s) acc
                    else acc)
        Map.empty
        allStocks
  in grouped
