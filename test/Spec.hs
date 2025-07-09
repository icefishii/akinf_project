{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

-- Import your CSV test functions
import AkinfProject.CSV (parseStocksFromBytes, Stock(..))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V

-- Import your Config types and YAML parsing
import Data.Yaml (decodeFileEither)
import AkinfProject.Config (Config(..), Timeframe(..))

-- Import Calculate module for testing
import AkinfProject.Calculate (StockAnalysis(..), percentageChange, analyzeStock, analyzeAllStocks, 
                                TrendDirection(..), RiskLevel(..), dailyVolatility, calculateROI, 
                                detectTrend, calculateRiskLevel)
import qualified Data.Map.Strict as Map

-- Import Output module for testing
import AkinfProject.Output (formatPercentage)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ csvTests
  , configTests
  , calculateTests
  , outputTests
  , integrationTests
  ]

-- CSV Parsing tests
csvTests :: TestTree
csvTests = testGroup "CSV Parsing Tests"
  [ testCase "Parses valid CSV with one row" $ 
      let csv = BL.unlines
                [ "date,open,high,low,close,volume,Name"
                , "2013-02-08,15.07,15.12,14.63,14.75,8407500,AAL"
                ]
          result = parseStocksFromBytes csv
      in case result of
          Left err -> assertFailure ("Expected success, got error: " ++ err)
          Right vec -> do
            length vec @?= 1
            let stock = V.head vec
            name stock @?= "AAL"
            open stock @?= Just 15.07

  , testCase "Fails on missing headers" $
      let badCsv = BL.unlines
                   [ "wrongheader1,wrongheader2"
                   , "1,2"
                   ]
      in case parseStocksFromBytes badCsv of
          Left _ -> return ()
          Right _ -> assertFailure "Expected failure, but parsing succeeded"
  ]

-- YAML Config parsing tests
configTests :: TestTree
configTests = testGroup "YAML Config Parsing Tests"
  [ testCase "Load valid YAML config" $ do
      result <- decodeFileEither "test/test-config.yaml"
      case result of
        Left err -> assertFailure ("Failed to parse YAML config: " ++ show err)
        Right (config :: Config) -> do
          stocks config @?= ["AAPL", "MSFT", "GOOG"]
          let tf = timeframe config
          start_date tf @?= "2020-01-01"
          end_date tf @?= "2023-01-01"
  ]

-- Calculate module tests
calculateTests :: TestTree
calculateTests = testGroup "Calculate Module Tests"
  [ testCase "percentageChange calculates correctly" $ do
      let stock1 = Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"
          stock2 = Stock "2023-01-02" (Just 50.0) (Just 55.0) (Just 45.0) (Just 48.0) (Just 2000) "TEST"
          stock3 = Stock "2023-01-03" Nothing Nothing Nothing Nothing Nothing "TEST"
      
      percentageChange stock1 @?= Just 2.0  -- (102-100)/100 * 100
      percentageChange stock2 @?= Just (-4.0)  -- (48-50)/50 * 100
      percentageChange stock3 @?= Nothing  -- Missing data

  , testCase "analyzeStock finds best and worst days" $ do
      let stocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "AAPL"  -- +2%
            , Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) (Just 107.1) (Just 1200) "AAPL" -- +5%
            , Stock "2023-01-03" (Just 107.1) (Just 109.0) (Just 105.0) (Just 106.0) (Just 800) "AAPL"  -- -1.03%
            , Stock "2023-01-04" (Just 106.0) (Just 108.0) (Just 103.0) (Just 103.18) (Just 900) "AAPL" -- -2.66%
            ]
          analysis = analyzeStock "AAPL" stocks
      
      stockName analysis @?= "AAPL"
      
      case bestDay analysis of
        Just (date, pct) -> do
          date @?= "2023-01-02"
          assertBool "Best day percentage should be around 5%" (abs (pct - 5.0) < 0.1)
        Nothing -> assertFailure "Should have found a best day"
      
      case worstDay analysis of
        Just (date, pct) -> do
          date @?= "2023-01-04"
          assertBool "Worst day percentage should be around -2.66%" (abs (pct - (-2.66)) < 0.1)
        Nothing -> assertFailure "Should have found a worst day"

  , testCase "analyzeStock handles empty vector" $ do
      let analysis = analyzeStock "EMPTY" V.empty
      stockName analysis @?= "EMPTY"
      bestDay analysis @?= Nothing
      worstDay analysis @?= Nothing

  , testCase "analyzeStock handles stocks with no valid price data" $ do
      let stocks = V.fromList
            [ Stock "2023-01-01" Nothing Nothing Nothing Nothing Nothing "TEST"
            , Stock "2023-01-02" Nothing Nothing Nothing Nothing Nothing "TEST"
            ]
          analysis = analyzeStock "TEST" stocks
      
      stockName analysis @?= "TEST"
      bestDay analysis @?= Nothing
      worstDay analysis @?= Nothing

  , testCase "analyzeAllStocks processes multiple stocks" $ do
      let stocksMap = Map.fromList
            [ ("AAPL", V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 103.0) (Just 1000) "AAPL"  -- +3%
                , Stock "2023-01-02" (Just 103.0) (Just 108.0) (Just 100.0) (Just 101.94) (Just 1200) "AAPL" -- -1.03%
                ])
            , ("MSFT", V.fromList
                [ Stock "2023-01-01" (Just 200.0) (Just 210.0) (Just 195.0) (Just 208.0) (Just 500) "MSFT"   -- +4%
                , Stock "2023-01-02" (Just 208.0) (Just 212.0) (Just 202.0) (Just 204.16) (Just 600) "MSFT"  -- -1.85%
                ])
            ]
          analyses = analyzeAllStocks stocksMap
      
      length analyses @?= 2
      
      let aaplAnalysis = head [a | a <- analyses, stockName a == "AAPL"]
          msftAnalysis = head [a | a <- analyses, stockName a == "MSFT"]
      
      stockName aaplAnalysis @?= "AAPL"
      stockName msftAnalysis @?= "MSFT"
      
      -- Check that both stocks have best and worst days
      assertBool "AAPL should have best day" (case bestDay aaplAnalysis of Just _ -> True; Nothing -> False)
      assertBool "AAPL should have worst day" (case worstDay aaplAnalysis of Just _ -> True; Nothing -> False)
      assertBool "MSFT should have best day" (case bestDay msftAnalysis of Just _ -> True; Nothing -> False)
      assertBool "MSFT should have worst day" (case worstDay msftAnalysis of Just _ -> True; Nothing -> False)

  , testCase "analyzeAllStocks handles empty map" $ do
      let analyses = analyzeAllStocks Map.empty
      length analyses @?= 0

  , testCase "dailyVolatility calculates correctly" $ do
      let stock1 = Stock "2023-01-01" (Just 100.0) (Just 110.0) (Just 95.0) (Just 105.0) (Just 1000) "TEST"  -- (110-95)/100 = 15%
          stock2 = Stock "2023-01-02" (Just 50.0) (Just 52.0) (Just 48.0) (Just 49.0) (Just 2000) "TEST"    -- (52-48)/50 = 8%
          stock3 = Stock "2023-01-03" Nothing Nothing Nothing Nothing Nothing "TEST"                         -- No data
      
      case dailyVolatility stock1 of
        Just vol -> assertBool "Volatility should be 15%" (abs (vol - 15.0) < 0.1)
        Nothing -> assertFailure "Should calculate volatility"
      
      case dailyVolatility stock2 of
        Just vol -> assertBool "Volatility should be 8%" (abs (vol - 8.0) < 0.1)
        Nothing -> assertFailure "Should calculate volatility"
      
      dailyVolatility stock3 @?= Nothing

  , testCase "calculateROI works correctly" $ do
      let stocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"
            , Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) (Just 107.0) (Just 1200) "TEST"
            , Stock "2023-01-03" (Just 107.0) (Just 109.0) (Just 105.0) (Just 108.0) (Just 800) "TEST"
            ]
      case calculateROI stocks of
        Just roi -> assertBool "ROI should be around 5.88%" (abs (roi - 5.88) < 1.0)  -- (108-102)/102 * 100
        Nothing -> assertFailure "Should calculate ROI"
      
      -- Test empty vector
      calculateROI V.empty @?= Nothing
      
      -- Test single stock (should be 0% ROI)
      let singleStock = V.fromList [Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"]
      calculateROI singleStock @?= Just 0.0

  , testCase "detectTrend classifies trends correctly" $ do
      let upwardStocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"
            , Stock "2023-01-03" (Just 102.0) (Just 109.0) (Just 105.0) (Just 112.0) (Just 800) "TEST"  -- +9.8% ROI
            ]
          downwardStocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"
            , Stock "2023-01-03" (Just 102.0) (Just 105.0) (Just 90.0) (Just 92.0) (Just 800) "TEST"    -- -9.8% ROI
            ]
          sidewaysStocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"
            , Stock "2023-01-03" (Just 102.0) (Just 105.0) (Just 99.0) (Just 103.0) (Just 800) "TEST"   -- +0.98% ROI
            ]
      
      detectTrend upwardStocks @?= Just Upward
      detectTrend downwardStocks @?= Just Downward
      detectTrend sidewaysStocks @?= Just Sideways

  , testCase "calculateRiskLevel assesses risk correctly" $ do
      let lowRiskStocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 102.0) (Just 99.0) (Just 101.0) (Just 1000) "TEST"   -- 3% volatility
            , Stock "2023-01-02" (Just 101.0) (Just 103.0) (Just 100.0) (Just 102.0) (Just 1200) "TEST"  -- ~3% volatility
            ]
          highRiskStocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 115.0) (Just 85.0) (Just 105.0) (Just 1000) "TEST"   -- 30% volatility
            , Stock "2023-01-02" (Just 105.0) (Just 120.0) (Just 90.0) (Just 110.0) (Just 1200) "TEST"   -- ~28% volatility
            ]
      
      let (lowVol, lowRisk) = calculateRiskLevel lowRiskStocks
          (highVol, highRisk) = calculateRiskLevel highRiskStocks
      
      lowRisk @?= Just Low
      highRisk @?= Just High
      
      case lowVol of
        Just vol -> assertBool "Low volatility should be under 4%" (vol < 4.0)
        Nothing -> assertFailure "Should calculate volatility"
      
      case highVol of
        Just vol -> assertBool "High volatility should be over 8%" (vol > 8.0)
        Nothing -> assertFailure "Should calculate volatility"

  , testCase "analyzeStock includes all new metrics" $ do
      let stocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "AAPL"  -- +2%
            , Stock "2023-01-02" (Just 102.0) (Just 112.0) (Just 98.0) (Just 108.0) (Just 1200) "AAPL" -- +5.88%
            , Stock "2023-01-03" (Just 108.0) (Just 110.0) (Just 105.0) (Just 106.0) (Just 800) "AAPL"  -- -1.85%
            ]
          analysis = analyzeStock "AAPL" stocks
      
      stockName analysis @?= "AAPL"
      
      -- Check that all metrics are calculated
      assertBool "Should have ROI" (case totalROI analysis of Just _ -> True; Nothing -> False)
      assertBool "Should have trend" (case trendDirection analysis of Just _ -> True; Nothing -> False)
      assertBool "Should have risk level" (case riskLevel analysis of Just _ -> True; Nothing -> False)
      assertBool "Should have volatility" (case volatility analysis of Just _ -> True; Nothing -> False)
  ]

-- Output module tests
outputTests :: TestTree
outputTests = testGroup "Output Module Tests"
  [ testCase "formatPercentage formats correctly" $ do
      formatPercentage 5.0 @?= "5.00%"
      formatPercentage (-3.14159) @?= "-3.14%"
      formatPercentage 0.0 @?= "0.00%"
      formatPercentage 10.666 @?= "10.67%"
      formatPercentage (-0.01) @?= "-0.01%"
  ]

-- Integration tests
integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testCase "Full workflow with valid stock data" $ do
      let stocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 103.0) (Just 1000) "AAPL"  -- +3%
            , Stock "2023-01-02" (Just 103.0) (Just 108.0) (Just 100.0) (Just 96.47) (Just 1200) "AAPL" -- -6.35%
            , Stock "2023-01-03" (Just 96.47) (Just 99.0) (Just 95.0) (Just 98.0) (Just 800) "AAPL"     -- +1.59%
            ]
          stockMap = Map.singleton "AAPL" stocks
          analyses = analyzeAllStocks stockMap
          
      length analyses @?= 1
      let analysis = head analyses
      stockName analysis @?= "AAPL"
      
      case bestDay analysis of
        Just (date, pct) -> do
          date @?= "2023-01-01"
          assertBool "Best day should be around 3%" (abs (pct - 3.0) < 0.1)
        Nothing -> assertFailure "Should have found a best day"
      
      case worstDay analysis of
        Just (date, pct) -> do
          date @?= "2023-01-02"
          assertBool "Worst day should be around -6.35%" (abs (pct - (-6.35)) < 0.1)
        Nothing -> assertFailure "Should have found a worst day"
      
      -- Check new metrics are present
      assertBool "Should have ROI" (case totalROI analysis of Just _ -> True; Nothing -> False)
      assertBool "Should have trend" (case trendDirection analysis of Just _ -> True; Nothing -> False)
      assertBool "Should have risk" (case riskLevel analysis of Just _ -> True; Nothing -> False)
      assertBool "Should have volatility" (case volatility analysis of Just _ -> True; Nothing -> False)

  , testCase "Handles mixed valid and invalid stock data" $ do
      let stocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 105.0) (Just 1000) "TEST"  -- +5%
            , Stock "2023-01-02" Nothing Nothing Nothing Nothing Nothing "TEST"                        -- No data
            , Stock "2023-01-03" (Just 105.0) (Just 110.0) (Just 100.0) (Just 102.9) (Just 800) "TEST" -- -2%
            ]
          stockMap = Map.singleton "TEST" stocks
          analyses = analyzeAllStocks stockMap
          
      length analyses @?= 1
      let analysis = head analyses
      stockName analysis @?= "TEST"
      
      -- Should find best and worst from valid entries only
      assertBool "Should have best day" (case bestDay analysis of Just _ -> True; Nothing -> False)
      assertBool "Should have worst day" (case worstDay analysis of Just _ -> True; Nothing -> False)

  , testCase "Edge case: Single stock entry" $ do
      let stocks = V.fromList
            [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "SINGLE"
            ]
          stockMap = Map.singleton "SINGLE" stocks
          analyses = analyzeAllStocks stockMap
          
      length analyses @?= 1
      let analysis = head analyses
      stockName analysis @?= "SINGLE"
      
      -- Both best and worst should be the same day
      case (bestDay analysis, worstDay analysis) of
        (Just (bestDate, bestPct), Just (worstDate, worstPct)) -> do
          bestDate @?= "2023-01-01"
          worstDate @?= "2023-01-01"
          bestPct @?= worstPct
        _ -> assertFailure "Should have found both best and worst day (same day)"
  ]
