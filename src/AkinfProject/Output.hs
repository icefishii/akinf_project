{-# LANGUAGE DeriveGeneric #-}
module AkinfProject.Output where

import AkinfProject.Calculate (StockAnalysis(..))
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
displayStockAnalysis (StockAnalysis stockName bestDay worstDay) = do
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
  
  putStrLn ""

-- | Format percentage change for display
formatPercentage :: Double -> String
formatPercentage pct = printf "%.2f%%" pct

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
      let allGains = [gain | StockAnalysis _ (Just (_, gain)) _ <- validAnalyses]
          allLosses = [loss | StockAnalysis _ _ (Just (_, loss)) <- validAnalyses]
      
      when (not (null allGains)) $ do
        let maxGain = maximum allGains
        putStrLn $ printf "  Highest single-day gain: %.2f%%" maxGain
      
      when (not (null allLosses)) $ do
        let maxLoss = minimum allLosses
        putStrLn $ printf "  Largest single-day loss: %.2f%%" maxLoss
    else
      putStrLn "  No valid data found for analysis."
  
  where
    hasValidData (StockAnalysis _ bestDay worstDay) = 
      case (bestDay, worstDay) of
        (Just _, _) -> True
        (_, Just _) -> True
        _ -> False
    
    when True action = action
    when False _ = return ()