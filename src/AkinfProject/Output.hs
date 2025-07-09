module AkinfProject.Output where

import AkinfProject.Calculate
  ( MASignal (..),
    MovingAverages (..),
    PortfolioCorrelation (..),
    RiskLevel (..),
    StockAnalysis (..),
    TrendDirection (..),
  )
import Control.Monad
import Data.Map qualified as Map
import Text.Printf (printf)

-- | Display analysis results for all stocks
displayAnalysis :: [StockAnalysis] -> IO ()
displayAnalysis analyses = do
  putStrLn "\n=========================================="
  putStrLn "         STOCK ANALYSIS RESULTS"
  putStrLn "=========================================="
  putStrLn ""

  if null analyses
    then putStrLn "No stock data to analyze."
    else mapM_ displayStockAnalysis analyses

  putStrLn "=========================================="

-- | Display analysis for a single stock
displayStockAnalysis :: StockAnalysis -> IO ()
displayStockAnalysis (StockAnalysis stockName bestDay worstDay roi trend risk volatility movingAvgs) = do
  putStrLn $ "Stock: " ++ stockName
  putStrLn $ "  " ++ replicate (length stockName + 6) '-'

  case bestDay of
    Nothing -> putStrLn "  Best day:  No data available"
    Just (date, gain) ->
      putStrLn $ printf "  Best day:  %s (%.2f%% gain)" date gain

  case worstDay of
    Nothing -> putStrLn "  Worst day: No data available"
    Just (date, loss) ->
      putStrLn $ printf "  Worst day: %s (%.2f%% loss)" date loss

  case roi of
    Nothing -> putStrLn "  Total ROI: No data available"
    Just roiValue ->
      putStrLn $ printf "  Total ROI: %.2f%%" roiValue

  case trend of
    Nothing -> putStrLn "  Trend:     No data available"
    Just trendValue ->
      putStrLn $ "  Trend:     " ++ show trendValue

  case volatility of
    Nothing -> putStrLn "  Volatility: No data available"
    Just volValue ->
      putStrLn $ printf "  Volatility: %.2f%% (daily avg)" volValue

  case risk of
    Nothing -> putStrLn "  Risk Level: No data available"
    Just riskValue ->
      putStrLn $ "  Risk Level: " ++ show riskValue ++ formatRiskDescription riskValue

  -- Display moving averages
  displayMovingAverages movingAvgs

  putStrLn ""

-- | Format percentage change for display
formatPercentage :: Double -> String
formatPercentage = printf "%.2f%%"

-- | Add descriptive text for risk levels
formatRiskDescription :: RiskLevel -> String
formatRiskDescription Low = " (Conservative investment)"
formatRiskDescription Medium = " (Moderate risk/reward)"
formatRiskDescription High = " (High risk/high reward)"

-- | Display moving averages analysis
displayMovingAverages :: MovingAverages -> IO ()
displayMovingAverages (MovingAverages sma20 sma50 sma200 signal) = do
  putStrLn "  Moving Averages:"
  case sma20 of
    Nothing -> putStrLn "    SMA 20:  No data available"
    Just val -> putStrLn $ printf "    SMA 20:  $%.2f" val

  case sma50 of
    Nothing -> putStrLn "    SMA 50:  No data available"
    Just val -> putStrLn $ printf "    SMA 50:  $%.2f" val

  case sma200 of
    Nothing -> putStrLn "    SMA 200: No data available"
    Just val -> putStrLn $ printf "    SMA 200: $%.2f" val

  case signal of
    Nothing -> putStrLn "    Signal:  No data available"
    Just sig -> putStrLn $ "    Signal:  " ++ show sig ++ formatSignalDescription sig

-- | Add descriptive text for MA signals
formatSignalDescription :: MASignal -> String
formatSignalDescription Bullish = " (Strong buy signal)"
formatSignalDescription Bearish = " (Strong sell signal)"
formatSignalDescription Neutral = " (Hold/neutral)"

-- | Display portfolio correlation analysis
displayPortfolioCorrelation :: PortfolioCorrelation -> IO ()
displayPortfolioCorrelation (PortfolioCorrelation corrMatrix avgCorr diversification) = do
  putStrLn "\n=========================================="
  putStrLn "       PORTFOLIO CORRELATION ANALYSIS"
  putStrLn "=========================================="

  putStrLn "\nStock Correlations:"
  if Map.null corrMatrix
    then putStrLn "  No correlation data available (need at least 2 stocks)"
    else do
      mapM_ displayCorrelationPair (Map.toList corrMatrix)
      putStrLn ""
      putStrLn $ printf "Average Correlation: %.3f" avgCorr
      putStrLn $ "Portfolio Diversification: " ++ diversification
      putStrLn $ "  " ++ interpretDiversification diversification

  putStrLn "=========================================="

-- | Display individual correlation pair
displayCorrelationPair :: ((String, String), Double) -> IO ()
displayCorrelationPair ((stock1, stock2), corr) =
  putStrLn $ printf "  %s <-> %s: %.3f %s" stock1 stock2 corr (interpretCorrelation corr)

-- | Interpret correlation strength
interpretCorrelation :: Double -> String
interpretCorrelation corr
  | abs corr > 0.8 = "(Very Strong)"
  | abs corr > 0.6 = "(Strong)"
  | abs corr > 0.4 = "(Moderate)"
  | abs corr > 0.2 = "(Weak)"
  | otherwise = "(Very Weak)"

-- | Interpret diversification level
interpretDiversification :: String -> String
interpretDiversification "Well Diversified" = "[Good] Good risk spread across different market movements"
interpretDiversification "Moderately Diversified" = "[Caution] Moderate risk spread, consider more diverse holdings"
interpretDiversification "Poorly Diversified" = "[Warning] High risk - stocks move together, limited protection"
interpretDiversification _ = "Unknown diversification level"

-- | Display summary statistics
displaySummary :: [StockAnalysis] -> IO ()
displaySummary analyses = do
  let validAnalyses = filter hasValidData analyses
      totalStocks = length analyses
      stocksWithData = length validAnalyses

  putStrLn "\nSUMMARY:"
  putStrLn $ "  Total stocks analyzed: " ++ show totalStocks
  putStrLn $ "  Stocks with valid data: " ++ show stocksWithData

  if not (null validAnalyses)
    then do
      let allGains = [gain | StockAnalysis _ (Just (_, gain)) _ _ _ _ _ _ <- validAnalyses]
          allLosses = [loss | StockAnalysis _ _ (Just (_, loss)) _ _ _ _ _ <- validAnalyses]
          allROIs = [roi | StockAnalysis _ _ _ (Just roi) _ _ _ _ <- validAnalyses]
          allVolatilities = [vol | StockAnalysis _ _ _ _ _ _ (Just vol) _ <- validAnalyses]

      unless (null allGains) $ do
        let maxGain = maximum allGains
        putStrLn $ printf "  Highest single-day gain: %.2f%%" maxGain

      unless (null allLosses) $ do
        let maxLoss = minimum allLosses
        putStrLn $ printf "  Largest single-day loss: %.2f%%" maxLoss

      unless (null allROIs) $ do
        let avgROI = sum allROIs / fromIntegral (length allROIs)
            bestROI = maximum allROIs
            worstROI = minimum allROIs
        putStrLn $ printf "  Average ROI: %.2f%%" avgROI
        putStrLn $ printf "  Best performing stock ROI: %.2f%%" bestROI
        putStrLn $ printf "  Worst performing stock ROI: %.2f%%" worstROI

      unless (null allVolatilities) $ do
        let avgVolatility = sum allVolatilities / fromIntegral (length allVolatilities)
        putStrLn $ printf "  Average volatility: %.2f%%" avgVolatility

      -- Risk distribution
      let riskCounts = foldr countRisks (0 :: Int, 0 :: Int, 0 :: Int) validAnalyses
      putStrLn $ "  Risk distribution: " ++ formatRiskDistribution riskCounts

      -- Trend distribution
      let trendCounts = foldr countTrends (0 :: Int, 0 :: Int, 0 :: Int) validAnalyses
      putStrLn $ "  Trend distribution: " ++ formatTrendDistribution trendCounts
    else
      putStrLn "  No valid data found for analysis."
  where
    hasValidData (StockAnalysis _ bestDay worstDay _ _ _ _ _) =
      case (bestDay, worstDay) of
        (Just _, _) -> True
        (_, Just _) -> True
        _ -> False

    countRisks (StockAnalysis _ _ _ _ _ (Just Low) _ _) (l, m, h) = (l + 1, m, h)
    countRisks (StockAnalysis _ _ _ _ _ (Just Medium) _ _) (l, m, h) = (l, m + 1, h)
    countRisks (StockAnalysis _ _ _ _ _ (Just High) _ _) (l, m, h) = (l, m, h + 1)
    countRisks _ counts = counts

    countTrends (StockAnalysis _ _ _ _ (Just Upward) _ _ _) (u, d, s) = (u + 1, d, s)
    countTrends (StockAnalysis _ _ _ _ (Just Downward) _ _ _) (u, d, s) = (u, d + 1, s)
    countTrends (StockAnalysis _ _ _ _ (Just Sideways) _ _ _) (u, d, s) = (u, d, s + 1)
    countTrends _ counts = counts

    formatRiskDistribution (low, med, high) =
      printf "Low: %d, Medium: %d, High: %d" low med high

    formatTrendDistribution (up, down, side) =
      printf "Upward: %d, Downward: %d, Sideways: %d" up down side
