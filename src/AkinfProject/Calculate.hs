{-# LANGUAGE DeriveGeneric #-}
module AkinfProject.Calculate where

import AkinfProject.CSV (Stock(..))
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

-- | Result type for a stock's best and worst day
data StockAnalysis = StockAnalysis
  { stockName :: String
  , bestDay :: Maybe (String, Double)  -- (date, percentage_gain)
  , worstDay :: Maybe (String, Double) -- (date, percentage_loss)
  } deriving (Show)

-- | Calculate percentage change from open to close
percentageChange :: Stock -> Maybe Double
percentageChange stock = do
  o <- open stock
  c <- close stock
  return $ ((c - o) / o) * 100

-- | Find the day with highest gain and highest loss for a single stock
analyzeStock :: String -> V.Vector Stock -> StockAnalysis
analyzeStock stockName stocks
  | V.null stocks = StockAnalysis stockName Nothing Nothing
  | otherwise = 
      let stocksWithChanges = V.mapMaybe (\s -> fmap (\pc -> (s, pc)) (percentageChange s)) stocks
      in if V.null stocksWithChanges
         then StockAnalysis stockName Nothing Nothing
         else
           let bestStock = V.maximumBy (comparing snd) stocksWithChanges
               worstStock = V.minimumBy (comparing snd) stocksWithChanges
               bestDay' = Just (date (fst bestStock), snd bestStock)
               worstDay' = Just (date (fst worstStock), snd worstStock)
           in StockAnalysis stockName bestDay' worstDay'

-- | Analyze all stocks in the filtered data
analyzeAllStocks :: Map.Map String (V.Vector Stock) -> [StockAnalysis]
analyzeAllStocks stockMap = 
  Map.foldrWithKey (\stockName stocks acc -> analyzeStock stockName stocks : acc) [] stockMap