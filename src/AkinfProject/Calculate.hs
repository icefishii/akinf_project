{-# LANGUAGE DeriveGeneric #-}
module AkinfProject.Calculate where

import AkinfProject.CSV (Stock(..))
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

-- | Trend direction enumeration
data TrendDirection = Upward | Downward | Sideways deriving (Show, Eq)

-- | Risk level enumeration
data RiskLevel = Low | Medium | High deriving (Show, Eq)

-- | Result type for a stock's comprehensive analysis
data StockAnalysis = StockAnalysis
  { stockName :: String
  , bestDay :: Maybe (String, Double)  -- (date, percentage_gain)
  , worstDay :: Maybe (String, Double) -- (date, percentage_loss)
  , totalROI :: Maybe Double           -- Return on Investment for timeframe
  , trendDirection :: Maybe TrendDirection -- Overall trend
  , riskLevel :: Maybe RiskLevel       -- Risk assessment
  , volatility :: Maybe Double         -- Average daily volatility percentage
  } deriving (Show)

-- | Calculate percentage change from open to close
percentageChange :: Stock -> Maybe Double
percentageChange stock = do
  o <- open stock
  c <- close stock
  return $ ((c - o) / o) * 100

-- | Calculate daily volatility as percentage difference between high and low
dailyVolatility :: Stock -> Maybe Double
dailyVolatility stock = do
  h <- high stock
  l <- low stock
  o <- open stock
  return $ ((h - l) / o) * 100

-- | Calculate total ROI from first close to last close
calculateROI :: V.Vector Stock -> Maybe Double
calculateROI stocks
  | V.null stocks = Nothing
  | otherwise = do
      let validStocks = V.filter (\s -> case close s of Just _ -> True; Nothing -> False) stocks
          sortedStocks = V.toList validStocks
      case sortedStocks of
        [] -> Nothing
        [_] -> do
          -- If only one stock, ROI is 0 (no change)
          return 0.0
        _ -> do
          let firstStock = head sortedStocks
              lastStock = last sortedStocks
          firstClose <- close firstStock
          lastClose <- close lastStock
          return $ ((lastClose - firstClose) / firstClose) * 100

-- | Detect trend direction based on first and last price
detectTrend :: V.Vector Stock -> Maybe TrendDirection
detectTrend stocks = do
  roi <- calculateROI stocks
  return $ if roi > 5.0 then Upward
           else if roi < -5.0 then Downward
           else Sideways

-- | Calculate average volatility and determine risk level
calculateRiskLevel :: V.Vector Stock -> (Maybe Double, Maybe RiskLevel)
calculateRiskLevel stocks
  | V.null stocks = (Nothing, Nothing)
  | otherwise = 
      let volatilities = V.mapMaybe dailyVolatility stocks
          avgVolatility = if V.null volatilities 
                         then Nothing 
                         else Just (V.sum volatilities / fromIntegral (V.length volatilities))
          riskLevel = case avgVolatility of
                       Nothing -> Nothing
                       Just avg -> Just $ if avg > 8.0 then High
                                         else if avg > 4.0 then Medium
                                         else Low
      in (avgVolatility, riskLevel)

-- | Find the day with highest gain and highest loss for a single stock
analyzeStock :: String -> V.Vector Stock -> StockAnalysis
analyzeStock stockName stocks
  | V.null stocks = StockAnalysis stockName Nothing Nothing Nothing Nothing Nothing Nothing
  | otherwise = 
      let stocksWithChanges = V.mapMaybe (\s -> fmap (\pc -> (s, pc)) (percentageChange s)) stocks
          (bestDay', worstDay') = if V.null stocksWithChanges
                                 then (Nothing, Nothing)
                                 else let bestStock = V.maximumBy (comparing snd) stocksWithChanges
                                          worstStock = V.minimumBy (comparing snd) stocksWithChanges
                                      in (Just (date (fst bestStock), snd bestStock),
                                          Just (date (fst worstStock), snd worstStock))
          
          -- Calculate new metrics
          roi = calculateROI stocks
          trend = detectTrend stocks
          (avgVolatility, risk) = calculateRiskLevel stocks
          
      in StockAnalysis stockName bestDay' worstDay' roi trend risk avgVolatility

-- | Analyze all stocks in the filtered data
analyzeAllStocks :: Map.Map String (V.Vector Stock) -> [StockAnalysis]
analyzeAllStocks stockMap = 
  Map.foldrWithKey (\stockName stocks acc -> analyzeStock stockName stocks : acc) [] stockMap