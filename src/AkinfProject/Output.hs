{-# LANGUAGE DeriveGeneric #-}
module AkinfProject.Output where

import AkinfProject.Calculate (StockAnalysis(..), TrendDirection(..), RiskLevel(..))
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
displayStockAnalysis (StockAnalysis stockName bestDay worstDay roi trend risk volatility) = do
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
  
  putStrLn ""

-- | Format percentage change for display
formatPercentage :: Double -> String
formatPercentage pct = printf "%.2f%%" pct

-- | Add descriptive text for risk levels
formatRiskDescription :: RiskLevel -> String
formatRiskDescription Low = " (Conservative investment)"
formatRiskDescription Medium = " (Moderate risk/reward)"
formatRiskDescription High = " (High risk/high reward)"

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
      let allGains = [gain | StockAnalysis _ (Just (_, gain)) _ _ _ _ _ <- validAnalyses]
          allLosses = [loss | StockAnalysis _ _ (Just (_, loss)) _ _ _ _ <- validAnalyses]
          allROIs = [roi | StockAnalysis _ _ _ (Just roi) _ _ _ <- validAnalyses]
          allVolatilities = [vol | StockAnalysis _ _ _ _ _ _ (Just vol) <- validAnalyses]
      
      when (not (null allGains)) $ do
        let maxGain = maximum allGains
        putStrLn $ printf "  Highest single-day gain: %.2f%%" maxGain
      
      when (not (null allLosses)) $ do
        let maxLoss = minimum allLosses
        putStrLn $ printf "  Largest single-day loss: %.2f%%" maxLoss
      
      when (not (null allROIs)) $ do
        let avgROI = sum allROIs / fromIntegral (length allROIs)
            bestROI = maximum allROIs
            worstROI = minimum allROIs
        putStrLn $ printf "  Average ROI: %.2f%%" avgROI
        putStrLn $ printf "  Best performing stock ROI: %.2f%%" bestROI
        putStrLn $ printf "  Worst performing stock ROI: %.2f%%" worstROI
      
      when (not (null allVolatilities)) $ do
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
    hasValidData (StockAnalysis _ bestDay worstDay _ _ _ _) = 
      case (bestDay, worstDay) of
        (Just _, _) -> True
        (_, Just _) -> True
        _ -> False
    
    countRisks (StockAnalysis _ _ _ _ _ (Just Low) _) (l, m, h) = (l+1, m, h)
    countRisks (StockAnalysis _ _ _ _ _ (Just Medium) _) (l, m, h) = (l, m+1, h)
    countRisks (StockAnalysis _ _ _ _ _ (Just High) _) (l, m, h) = (l, m, h+1)
    countRisks _ counts = counts
    
    countTrends (StockAnalysis _ _ _ _ (Just Upward) _ _) (u, d, s) = (u+1, d, s)
    countTrends (StockAnalysis _ _ _ _ (Just Downward) _ _) (u, d, s) = (u, d+1, s)
    countTrends (StockAnalysis _ _ _ _ (Just Sideways) _ _) (u, d, s) = (u, d, s+1)
    countTrends _ counts = counts
    
    formatRiskDistribution (low, med, high) = 
      printf "Low: %d, Medium: %d, High: %d" low med high
    
    formatTrendDistribution (up, down, side) = 
      printf "Upward: %d, Downward: %d, Sideways: %d" up down side
    
    when True action = action
    when False _ = return ()