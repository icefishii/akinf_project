-- Let's us use String Literals for more then just String Type
{-# LANGUAGE OverloadedStrings #-}

module AkinfProject.Filter (filterByConfig) where

import AkinfProject.CSV (Stock (..))
import AkinfProject.Config (Config (..), Timeframe (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Vector qualified as V

-- Normalize stock names to uppercase and trim whitespace using Text
normalize :: T.Text -> T.Text
normalize = T.strip . T.toUpper

-- Helper function to convert String config values to Text for comparison
normalizeString :: String -> T.Text
normalizeString = T.strip . T.toUpper . T.pack

-- Parse date in "YYYY-MM-DD" format using a String
parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Parse Text date into Day using Text
parseDayText :: T.Text -> Maybe Day
parseDayText = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

-- Filter the stock data by config's stock list and date range
filterByConfig :: Config -> V.Vector Stock -> Map.Map String (V.Vector Stock)
filterByConfig (Config stockNames (Timeframe start end)) allStocks =
  -- First parse the dates to Maybe Day and normalize all stock name
  let startDay = parseDay start
      endDay = parseDay end
      normalizedNames = map normalizeString stockNames

      -- Check if the Stock's date is in Range of the Configs Date Range
      inRange stock = case parseDayText (date stock) of
        Just date -> case (startDay, endDay) of
          (Just startDate, Just endDate) -> startDate <= date && date <= endDate
          _ -> False
        _ -> False

      wanted stock = normalize (name stock) `elem` normalizedNames && inRange stock

      -- Group stocks by name and filter them to
      -- only include those within the specified date range
      groupedStocks =
        V.foldr
          ( \stock acc ->
              if wanted stock
                then Map.insertWith (V.++) (T.unpack (name stock)) (V.singleton stock) acc
                else acc
          )
          Map.empty
          allStocks
   in groupedStocks
