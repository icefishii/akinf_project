{-# LANGUAGE DeriveGeneric #-}

module AkinfProject.Calculate where

-- TODO: remove warnings
import AkinfProject.CSV (Stock (..))
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Vector qualified as V

-- | Trend direction enumeration
data TrendDirection = Upward | Downward | Sideways deriving (Show, Eq)

-- | Risk level enumeration
data RiskLevel = Low | Medium | High deriving (Show, Eq)

-- | Moving average signals
data MASignal = Bullish | Bearish | Neutral deriving (Show, Eq)

-- | Moving averages data
data MovingAverages = MovingAverages
  { sma20 :: Maybe Double,
    sma50 :: Maybe Double,
    sma200 :: Maybe Double,
    maSignal :: Maybe MASignal -- Based on SMA crossovers
  }
  deriving (Show)

-- | Result type for a stock's comprehensive analysis
data StockAnalysis = StockAnalysis
  { stockName :: String,
    bestDay :: Maybe (String, Double), -- (date, percentage_gain)
    worstDay :: Maybe (String, Double), -- (date, percentage_loss)
    totalROI :: Maybe Double, -- Return on Investment for timeframe
    trendDirection :: Maybe TrendDirection, -- Overall trend
    riskLevel :: Maybe RiskLevel, -- Risk assessment
    volatility :: Maybe Double, -- Average daily volatility percentage
    movingAverages :: MovingAverages -- SMA analysis
  }
  deriving (Show)

-- | Portfolio correlation data
data PortfolioCorrelation = PortfolioCorrelation
  { correlationMatrix :: Map.Map (String, String) Double,
    averageCorrelation :: Double,
    portfolioDiversification :: String -- "Well Diversified" | "Moderately Diversified" | "Poorly Diversified"
  }
  deriving (Show)

-- | Calculate percentage change from open to close
percentageChange :: Stock -> Maybe Double
percentageChange stock = do
  o <- open stock
  c <- close stock
  return $ ((c - o) / o) * 100

-- =============================================================================
-- MOVING AVERAGES MODULE
-- =============================================================================

-- | Calculate Simple Moving Average for given period
calculateSMA :: Int -> V.Vector Stock -> Maybe Double
calculateSMA period stocks
  | V.length stocks < period = Nothing
  | otherwise =
      let recentStocks = V.take period stocks
          validPrices = V.mapMaybe close recentStocks
       in if V.length validPrices == period
            then Just (V.sum validPrices / fromIntegral period)
            else Nothing

-- | Calculate all moving averages (20, 50, 200 day)
calculateMovingAverages :: V.Vector Stock -> MovingAverages
calculateMovingAverages stocks =
  let sma20' = calculateSMA 20 stocks
      sma50' = calculateSMA 50 stocks
      sma200' = calculateSMA 200 stocks
      signal = determineMASignal sma20' sma50' sma200'
   in MovingAverages sma20' sma50' sma200' signal

-- | Determine moving average signal based on crossovers
determineMASignal :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe MASignal
determineMASignal (Just sma20) (Just sma50) (Just sma200)
  | sma20 > sma50 && sma50 > sma200 = Just Bullish -- Golden Cross pattern
  | sma20 < sma50 && sma50 < sma200 = Just Bearish -- Death Cross pattern
  | otherwise = Just Neutral
determineMASignal _ _ _ = Nothing

-- =============================================================================
-- BASIC METRICS MODULE
-- =============================================================================

-- | Calculate daily volatility as percentage difference between high and low
dailyVolatility :: Stock -> Maybe Double
dailyVolatility stock = do
  h <- high stock
  l <- low stock
  o <- open stock
  return $ ((h - l) / o) * 100

-- =============================================================================
-- ROI AND TREND ANALYSIS MODULE
-- =============================================================================

-- | Calculate total ROI from first close to last close
calculateROI :: V.Vector Stock -> Maybe Double
calculateROI stocks
  | V.null stocks = Nothing
  | otherwise = do
      let validStocks = V.filter (\s -> case close s of Just _ -> True; Nothing -> False) stocks
          sortedStocks = V.toList validStocks
      case sortedStocks of
        [] -> Nothing
        [_] -> return 0.0 -- Single stock, no change
        _ -> do
          let firstStock = head sortedStocks
              lastStock = last sortedStocks
          firstClose <- close firstStock
          lastClose <- close lastStock
          return $ ((lastClose - firstClose) / firstClose) * 100

-- | Detect trend direction based on ROI
detectTrend :: V.Vector Stock -> Maybe TrendDirection
detectTrend stocks = do
  roi <- calculateROI stocks
  return $
    if roi > 5.0
      then Upward
      else
        if roi < -5.0
          then Downward
          else Sideways

-- =============================================================================
-- RISK ANALYSIS MODULE
-- =============================================================================

-- | Calculate average volatility and determine risk level
calculateRiskLevel :: V.Vector Stock -> (Maybe Double, Maybe RiskLevel)
calculateRiskLevel stocks
  | V.null stocks = (Nothing, Nothing)
  | otherwise =
      let volatilities = V.mapMaybe dailyVolatility stocks
          avgVolatility =
            if V.null volatilities
              then Nothing
              else Just (V.sum volatilities / fromIntegral (V.length volatilities))
          riskLevel = case avgVolatility of
            Nothing -> Nothing
            Just avg ->
              Just $
                if avg > 8.0
                  then High
                  else
                    if avg > 4.0
                      then Medium
                      else Low
       in (avgVolatility, riskLevel)

-- | Find the day with highest gain and highest loss for a single stock
analyzeStock :: String -> V.Vector Stock -> StockAnalysis
analyzeStock stockName stocks
  | V.null stocks =
      StockAnalysis
        stockName
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        (MovingAverages Nothing Nothing Nothing Nothing)
  | otherwise =
      let stocksWithChanges = V.mapMaybe (\s -> fmap (\pc -> (s, pc)) (percentageChange s)) stocks
          (bestDay', worstDay') =
            if V.null stocksWithChanges
              then (Nothing, Nothing)
              else
                let bestStock = V.maximumBy (comparing snd) stocksWithChanges
                    worstStock = V.minimumBy (comparing snd) stocksWithChanges
                 in ( Just (T.unpack (date (fst bestStock)), snd bestStock),
                      Just (T.unpack (date (fst worstStock)), snd worstStock)
                    )

          -- Calculate new metrics
          roi = calculateROI stocks
          trend = detectTrend stocks
          (avgVolatility, risk) = calculateRiskLevel stocks
          movingAverages = calculateMovingAverages stocks
       in StockAnalysis stockName bestDay' worstDay' roi trend risk avgVolatility movingAverages

-- =============================================================================
-- PORTFOLIO CORRELATION MODULE
-- =============================================================================

-- | Calculate daily returns for a stock
calculateDailyReturns :: V.Vector Stock -> V.Vector Double
calculateDailyReturns stocks =
  let stocksList = V.toList stocks
      pairs = zip stocksList (tail stocksList)
      returns = map calculateReturn pairs
   in V.fromList (catMaybes returns)
  where
    calculateReturn (prev, curr) = do
      prevClose <- close prev
      currClose <- close curr
      return $ (currClose - prevClose) / prevClose

    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\mx acc -> case mx of Just x -> x : acc; Nothing -> acc) []

-- | Calculate correlation coefficient between two return series
calculateCorrelation :: V.Vector Double -> V.Vector Double -> Maybe Double
calculateCorrelation returns1 returns2
  | V.length returns1 /= V.length returns2 || V.null returns1 = Nothing
  | otherwise =
      let n = fromIntegral (V.length returns1)
          mean1 = V.sum returns1 / n
          mean2 = V.sum returns2 / n

          numerator = V.sum $ V.zipWith (\x y -> (x - mean1) * (y - mean2)) returns1 returns2
          sumSq1 = V.sum $ V.map (\x -> (x - mean1) ^ (2 :: Int)) returns1
          sumSq2 = V.sum $ V.map (\x -> (x - mean2) ^ (2 :: Int)) returns2
          denominator = sqrt (sumSq1 * sumSq2)
       in if denominator == 0
            then Nothing
            else Just (numerator / denominator)

-- | Calculate portfolio correlation matrix
calculatePortfolioCorrelation :: Map.Map String (V.Vector Stock) -> PortfolioCorrelation
calculatePortfolioCorrelation stockMap =
  let stockNames = Map.keys stockMap
      stockReturns = Map.map calculateDailyReturns stockMap

      -- Calculate all pairwise correlations
      correlations =
        [ (name1, name2, corr)
          | name1 <- stockNames,
            name2 <- stockNames,
            name1 < name2, -- Avoid duplicates and self-correlation
            let returns1 = Map.findWithDefault V.empty name1 stockReturns,
            let returns2 = Map.findWithDefault V.empty name2 stockReturns,
            let corr = calculateCorrelation returns1 returns2,
            corr /= Nothing
        ]

      corrMap = Map.fromList [((name1, name2), fromJust corr) | (name1, name2, corr) <- correlations]

      avgCorr =
        if null correlations
          then 0.0
          else sum [fromJust corr | (_, _, corr) <- correlations] / fromIntegral (length correlations)

      diversification = classifyDiversification avgCorr
   in PortfolioCorrelation corrMap avgCorr diversification
  where
    fromJust (Just x) = x
    fromJust Nothing = 0.0 -- This shouldn't happen given our filter above
    classifyDiversification avg
      | avg < 0.3 = "Well Diversified"
      | avg < 0.7 = "Moderately Diversified"
      | otherwise = "Poorly Diversified"

-- | Analyze all stocks in the filtered data
analyzeAllStocks :: Map.Map String (V.Vector Stock) -> [StockAnalysis]
analyzeAllStocks stockMap =
  Map.foldrWithKey (\stockName stocks acc -> analyzeStock stockName stocks : acc) [] stockMap

-- | Comprehensive portfolio analysis including correlations
analyzePortfolio :: Map.Map String (V.Vector Stock) -> ([StockAnalysis], PortfolioCorrelation)
analyzePortfolio stockMap =
  let stockAnalyses = analyzeAllStocks stockMap
      correlationAnalysis = calculatePortfolioCorrelation stockMap
   in (stockAnalyses, correlationAnalysis)
