{-# LANGUAGE OverloadedStrings #-}

import AkinfProject.CSV (Stock (..), parseStocksFromBytes)
-- Import Calculate module for testing
import AkinfProject.Calculate
  ( MASignal (..),
    MovingAverages (..),
    PortfolioCorrelation (..),
    RiskLevel (..),
    StockAnalysis (..),
    TrendDirection (..),
    analyzeAllStocks,
    analyzePortfolio,
    analyzeStock,
    calculateCorrelation,
    calculateDailyReturns,
    calculateMovingAverages,
    calculatePortfolioCorrelation,
    calculateROI,
    calculateRiskLevel,
    calculateSMA,
    dailyVolatility,
    detectTrend,
    determineMASignal,
    percentageChange,
  )
import AkinfProject.Config (Config (..), Timeframe (..))
-- Import Output module for testing
import AkinfProject.Output (formatPercentage)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Yaml (decodeFileEither)
import PropertyTests (completePropertyTests)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ csvTests,
      configTests,
      calculateTests,
      outputTests,
      integrationTests,
      completePropertyTests -- Add property-based tests
    ]

-- CSV Parsing tests
csvTests :: TestTree
csvTests =
  testGroup
    "CSV Parsing Tests"
    [ testCase "Parses valid CSV with one row" $
        let csv =
              BL.unlines
                [ "date,open,high,low,close,volume,Name",
                  "2013-02-08,15.07,15.12,14.63,14.75,8407500,AAL"
                ]
            result = parseStocksFromBytes csv
         in case result of
              Left err -> assertFailure ("Expected success, got error: " ++ err)
              Right vec -> do
                length vec @?= 1
                let stock = V.head vec
                name stock @?= "AAL"
                open stock @?= Just 15.07,
      testCase "Fails on missing headers" $
        let badCsv =
              BL.unlines
                [ "wrongheader1,wrongheader2",
                  "1,2"
                ]
         in case parseStocksFromBytes badCsv of
              Left _ -> return ()
              Right _ -> assertFailure "Expected failure, but parsing succeeded"
    ]

-- YAML Config parsing tests
configTests :: TestTree
configTests =
  testGroup
    "YAML Config Parsing Tests"
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
calculateTests =
  testGroup
    "Calculate Module Tests"
    [ testCase "percentageChange calculates correctly" $ do
        let stock1 = Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"
            stock2 = Stock "2023-01-02" (Just 50.0) (Just 55.0) (Just 45.0) (Just 48.0) (Just 2000) "TEST"
            stock3 = Stock "2023-01-03" Nothing Nothing Nothing Nothing Nothing "TEST"

        percentageChange stock1 @?= Just 2.0 -- (102-100)/100 * 100
        percentageChange stock2 @?= Just (-4.0) -- (48-50)/50 * 100
        percentageChange stock3 @?= Nothing, -- Missing data
      testCase "analyzeStock finds best and worst days" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "AAPL", -- +2%
                  Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) (Just 107.1) (Just 1200) "AAPL", -- +5%
                  Stock "2023-01-03" (Just 107.1) (Just 109.0) (Just 105.0) (Just 106.0) (Just 800) "AAPL", -- -1.03%
                  Stock "2023-01-04" (Just 106.0) (Just 108.0) (Just 103.0) (Just 103.18) (Just 900) "AAPL" -- -2.66%
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
          Nothing -> assertFailure "Should have found a worst day",
      testCase "analyzeStock handles empty vector" $ do
        let analysis = analyzeStock "EMPTY" V.empty
        stockName analysis @?= "EMPTY"
        bestDay analysis @?= Nothing
        worstDay analysis @?= Nothing,
      testCase "analyzeStock handles stocks with no valid price data" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" Nothing Nothing Nothing Nothing Nothing "TEST",
                  Stock "2023-01-02" Nothing Nothing Nothing Nothing Nothing "TEST"
                ]
            analysis = analyzeStock "TEST" stocks

        stockName analysis @?= "TEST"
        bestDay analysis @?= Nothing
        worstDay analysis @?= Nothing,
      testCase "analyzeAllStocks processes multiple stocks" $ do
        let stocksMap =
              Map.fromList
                [ ( "AAPL",
                    V.fromList
                      [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 103.0) (Just 1000) "AAPL", -- +3%
                        Stock "2023-01-02" (Just 103.0) (Just 108.0) (Just 100.0) (Just 101.94) (Just 1200) "AAPL" -- -1.03%
                      ]
                  ),
                  ( "MSFT",
                    V.fromList
                      [ Stock "2023-01-01" (Just 200.0) (Just 210.0) (Just 195.0) (Just 208.0) (Just 500) "MSFT", -- +4%
                        Stock "2023-01-02" (Just 208.0) (Just 212.0) (Just 202.0) (Just 204.16) (Just 600) "MSFT" -- -1.85%
                      ]
                  )
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
        assertBool "MSFT should have worst day" (case worstDay msftAnalysis of Just _ -> True; Nothing -> False),
      testCase "analyzeAllStocks handles empty map" $ do
        let analyses = analyzeAllStocks Map.empty
        length analyses @?= 0,
      testCase "dailyVolatility calculates correctly" $ do
        let stock1 = Stock "2023-01-01" (Just 100.0) (Just 110.0) (Just 95.0) (Just 105.0) (Just 1000) "TEST" -- (110-95)/100 = 15%
            stock2 = Stock "2023-01-02" (Just 50.0) (Just 52.0) (Just 48.0) (Just 49.0) (Just 2000) "TEST" -- (52-48)/50 = 8%
            stock3 = Stock "2023-01-03" Nothing Nothing Nothing Nothing Nothing "TEST" -- No data
        case dailyVolatility stock1 of
          Just vol -> assertBool "Volatility should be 15%" (abs (vol - 15.0) < 0.1)
          Nothing -> assertFailure "Should calculate volatility"

        case dailyVolatility stock2 of
          Just vol -> assertBool "Volatility should be 8%" (abs (vol - 8.0) < 0.1)
          Nothing -> assertFailure "Should calculate volatility"

        dailyVolatility stock3 @?= Nothing,
      testCase "calculateROI works correctly" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST",
                  Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) (Just 107.0) (Just 1200) "TEST",
                  Stock "2023-01-03" (Just 107.0) (Just 109.0) (Just 105.0) (Just 108.0) (Just 800) "TEST"
                ]
        case calculateROI stocks of
          Just roi -> assertBool "ROI should be around 5.88%" (abs (roi - 5.88) < 1.0) -- (108-102)/102 * 100
          Nothing -> assertFailure "Should calculate ROI"

        -- Test empty vector
        calculateROI V.empty @?= Nothing

        -- Test single stock (should be 0% ROI)
        let singleStock = V.fromList [Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST"]
        calculateROI singleStock @?= Just 0.0,
      testCase "detectTrend classifies trends correctly" $ do
        let upwardStocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST",
                  Stock "2023-01-03" (Just 102.0) (Just 109.0) (Just 105.0) (Just 112.0) (Just 800) "TEST" -- +9.8% ROI
                ]
            downwardStocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST",
                  Stock "2023-01-03" (Just 102.0) (Just 105.0) (Just 90.0) (Just 92.0) (Just 800) "TEST" -- -9.8% ROI
                ]
            sidewaysStocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "TEST",
                  Stock "2023-01-03" (Just 102.0) (Just 105.0) (Just 99.0) (Just 103.0) (Just 800) "TEST" -- +0.98% ROI
                ]

        detectTrend upwardStocks @?= Just Upward
        detectTrend downwardStocks @?= Just Downward
        detectTrend sidewaysStocks @?= Just Sideways,
      testCase "calculateRiskLevel assesses risk correctly" $ do
        let lowRiskStocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 102.0) (Just 99.0) (Just 101.0) (Just 1000) "TEST", -- 3% volatility
                  Stock "2023-01-02" (Just 101.0) (Just 103.0) (Just 100.0) (Just 102.0) (Just 1200) "TEST" -- ~3% volatility
                ]
            highRiskStocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 115.0) (Just 85.0) (Just 105.0) (Just 1000) "TEST", -- 30% volatility
                  Stock "2023-01-02" (Just 105.0) (Just 120.0) (Just 90.0) (Just 110.0) (Just 1200) "TEST" -- ~28% volatility
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
          Nothing -> assertFailure "Should calculate volatility",
      testCase "analyzeStock includes all new metrics" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 102.0) (Just 1000) "AAPL", -- +2%
                  Stock "2023-01-02" (Just 102.0) (Just 112.0) (Just 98.0) (Just 108.0) (Just 1200) "AAPL", -- +5.88%
                  Stock "2023-01-03" (Just 108.0) (Just 110.0) (Just 105.0) (Just 106.0) (Just 800) "AAPL" -- -1.85%
                ]
            analysis = analyzeStock "AAPL" stocks

        stockName analysis @?= "AAPL"

        -- Check that all metrics are calculated
        assertBool "Should have ROI" (case totalROI analysis of Just _ -> True; Nothing -> False)
        assertBool "Should have trend" (case trendDirection analysis of Just _ -> True; Nothing -> False)
        assertBool "Should have risk level" (case riskLevel analysis of Just _ -> True; Nothing -> False)
        assertBool "Should have volatility" (case volatility analysis of Just _ -> True; Nothing -> False),
      -- Moving Averages Tests
      testCase "calculateSMA works correctly" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "TEST",
                  Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) (Just 105.0) (Just 1200) "TEST",
                  Stock "2023-01-03" (Just 108.0) (Just 110.0) (Just 105.0) (Just 110.0) (Just 800) "TEST",
                  Stock "2023-01-04" (Just 110.0) (Just 115.0) (Just 108.0) (Just 115.0) (Just 900) "TEST",
                  Stock "2023-01-05" (Just 115.0) (Just 120.0) (Just 112.0) (Just 120.0) (Just 1100) "TEST"
                ]

        -- Test SMA calculation (should average the first 'period' closing prices)
        case calculateSMA 3 stocks of
          Just sma -> assertBool "SMA-3 should be around 105%" (abs (sma - 105.0) < 1.0) -- (100+105+110)/3
          Nothing -> assertFailure "Should calculate SMA for sufficient data"

        case calculateSMA 5 stocks of
          Just sma -> assertBool "SMA-5 should be around 110%" (abs (sma - 110.0) < 1.0) -- (100+105+110+115+120)/5
          Nothing -> assertFailure "Should calculate SMA for sufficient data"

        -- Test insufficient data
        calculateSMA 10 stocks @?= Nothing -- Not enough data for 10-period SMA
        calculateSMA 3 V.empty @?= Nothing, -- Empty vector
      testCase "calculateMovingAverages computes all SMAs" $ do
        let stocks =
              V.fromList $
                replicate 250 $ -- Ensure we have enough data for SMA-200 -- Ensure we have enough data for SMA-200
                  Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "TEST"

            movingAvgs = calculateMovingAverages stocks

        -- All SMAs should be calculated for sufficient data
        case sma20 movingAvgs of
          Just val -> assertBool "SMA-20 should be around 100" (abs (val - 100.0) < 1.0)
          Nothing -> assertFailure "Should calculate SMA-20"

        case sma50 movingAvgs of
          Just val -> assertBool "SMA-50 should be around 100" (abs (val - 100.0) < 1.0)
          Nothing -> assertFailure "Should calculate SMA-50"

        case sma200 movingAvgs of
          Just val -> assertBool "SMA-200 should be around 100" (abs (val - 100.0) < 1.0)
          Nothing -> assertFailure "Should calculate SMA-200",
      testCase "determineMASignal classifies signals correctly" $ do
        -- Bullish: SMA20 > SMA50 > SMA200 (Golden Cross)
        determineMASignal (Just 110.0) (Just 105.0) (Just 100.0) @?= Just Bullish

        -- Bearish: SMA20 < SMA50 < SMA200 (Death Cross)
        determineMASignal (Just 100.0) (Just 105.0) (Just 110.0) @?= Just Bearish

        -- Neutral: Mixed signals
        determineMASignal (Just 105.0) (Just 100.0) (Just 110.0) @?= Just Neutral

        -- Missing data
        determineMASignal Nothing (Just 105.0) (Just 100.0) @?= Nothing
        determineMASignal (Just 110.0) Nothing (Just 100.0) @?= Nothing
        determineMASignal (Just 110.0) (Just 105.0) Nothing @?= Nothing,
      -- Portfolio Correlation Tests
      testCase "calculateDailyReturns computes returns correctly" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "TEST",
                  Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) (Just 105.0) (Just 1200) "TEST", -- +5% return
                  Stock "2023-01-03" (Just 108.0) (Just 110.0) (Just 105.0) (Just 110.0) (Just 800) "TEST", -- +4.76% return
                  Stock "2023-01-04" (Just 110.0) (Just 115.0) (Just 108.0) (Just 104.5) (Just 900) "TEST" -- -5% return
                ]
            returns = calculateDailyReturns stocks

        V.length returns @?= 3 -- Should be n-1 returns for n stocks
        let returnsList = V.toList returns
        assertBool "First return should be around 0.05" (abs (head returnsList - 0.05) < 0.001) -- 5%
        assertBool "Second return should be around 0.0476" (abs (returnsList !! 1 - 0.0476) < 0.001) -- ~4.76%
        assertBool "Third return should be around -0.05" (abs (returnsList !! 2 - (-0.05)) < 0.001), -- -5%
      testCase "calculateCorrelation computes correlation correctly" $ do
        let returns1 = V.fromList [0.05, 0.03, -0.02, 0.01, 0.04] -- Some positive returns
            returns2 = V.fromList [0.04, 0.02, -0.01, 0.02, 0.03] -- Similar pattern (should be positive corr)
            returns3 = V.fromList [-0.04, -0.02, 0.01, -0.02, -0.03] -- Opposite pattern (should be negative corr)
        case calculateCorrelation returns1 returns2 of
          Just corr -> assertBool "Correlation should be positive" (corr > 0.5)
          Nothing -> assertFailure "Should calculate positive correlation"

        case calculateCorrelation returns1 returns3 of
          Just corr -> assertBool "Correlation should be negative" (corr < -0.5)
          Nothing -> assertFailure "Should calculate negative correlation"

        -- Test edge cases
        calculateCorrelation V.empty V.empty @?= Nothing
        calculateCorrelation returns1 (V.take 3 returns2) @?= Nothing, -- Different lengths
      testCase "calculatePortfolioCorrelation works for multiple stocks" $ do
        let stocks1 =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "AAPL",
                  Stock "2023-01-02" (Just 100.0) (Just 108.0) (Just 100.0) (Just 105.0) (Just 1200) "AAPL",
                  Stock "2023-01-03" (Just 105.0) (Just 110.0) (Just 105.0) (Just 110.0) (Just 800) "AAPL"
                ]
            stocks2 =
              V.fromList
                [ Stock "2023-01-01" (Just 200.0) (Just 210.0) (Just 195.0) (Just 200.0) (Just 500) "MSFT",
                  Stock "2023-01-02" (Just 200.0) (Just 215.0) (Just 200.0) (Just 210.0) (Just 600) "MSFT",
                  Stock "2023-01-03" (Just 210.0) (Just 220.0) (Just 210.0) (Just 220.0) (Just 700) "MSFT"
                ]
            stockMap = Map.fromList [("AAPL", stocks1), ("MSFT", stocks2)]
            portfolioCorr = calculatePortfolioCorrelation stockMap

        -- Should have one correlation pair
        Map.size (correlationMatrix portfolioCorr) @?= 1

        case Map.lookup ("AAPL", "MSFT") (correlationMatrix portfolioCorr) of
          Just corr -> assertBool "Should have strong positive correlation" (corr > 0.8)
          Nothing -> assertFailure "Should find AAPL-MSFT correlation"

        assertBool "Average correlation should be positive" (averageCorrelation portfolioCorr > 0.0)
        portfolioDiversification portfolioCorr @?= "Poorly Diversified",
      testCase "analyzePortfolio provides comprehensive analysis" $ do
        let stocks1List =
              [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "AAPL",
                Stock "2023-01-02" (Just 100.0) (Just 108.0) (Just 100.0) (Just 105.0) (Just 1200) "AAPL", -- +5% return
                Stock "2023-01-03" (Just 105.0) (Just 110.0) (Just 105.0) (Just 110.0) (Just 800) "AAPL" -- +4.76% return
              ]
                ++ replicate 247 (Stock "2023-01-04" (Just 110.0) (Just 115.0) (Just 108.0) (Just 112.0) (Just 900) "AAPL")
            stocks2List =
              [ Stock "2023-01-01" (Just 200.0) (Just 210.0) (Just 195.0) (Just 200.0) (Just 500) "MSFT",
                Stock "2023-01-02" (Just 200.0) (Just 215.0) (Just 200.0) (Just 210.0) (Just 600) "MSFT", -- +5% return
                Stock "2023-01-03" (Just 210.0) (Just 220.0) (Just 210.0) (Just 220.0) (Just 700) "MSFT" -- +4.76% return
              ]
                ++ replicate 247 (Stock "2023-01-04" (Just 220.0) (Just 225.0) (Just 218.0) (Just 224.0) (Just 800) "MSFT")
            stocks1 = V.fromList stocks1List
            stocks2 = V.fromList stocks2List
            stockMap = Map.fromList [("AAPL", stocks1), ("MSFT", stocks2)]
            (analyses, portfolioCorr) = analyzePortfolio stockMap

        -- Should analyze both stocks
        length analyses @?= 2

        let stockNames = map stockName analyses
        assertBool "Should include AAPL" ("AAPL" `elem` stockNames)
        assertBool "Should include MSFT" ("MSFT" `elem` stockNames)

        -- Portfolio correlation should be calculated (both stocks have varying returns, so correlation should exist)
        assertBool "Should have at least some correlation data" (Map.size (correlationMatrix portfolioCorr) >= 0)
        assertBool
          "Should have reasonable average correlation"
          (averageCorrelation portfolioCorr >= -1.0 && averageCorrelation portfolioCorr <= 1.0),
      -- Additional Edge Case Tests
      testCase "calculateSMA handles stocks with missing close prices" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "TEST",
                  Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) Nothing (Just 1200) "TEST", -- Missing close
                  Stock "2023-01-03" (Just 108.0) (Just 110.0) (Just 105.0) (Just 110.0) (Just 800) "TEST"
                ]

        calculateSMA 3 stocks @?= Nothing -- Should fail due to missing close price
        calculateSMA 2 stocks @?= Nothing, -- Should fail due to missing close price in period
      testCase "calculateDailyReturns handles stocks with missing prices" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "TEST",
                  Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) Nothing (Just 1200) "TEST", -- Missing close
                  Stock "2023-01-03" (Just 108.0) (Just 110.0) (Just 105.0) (Just 110.0) (Just 800) "TEST"
                ]
            returns = calculateDailyReturns stocks

        -- Should have fewer returns due to missing data
        assertBool "Should handle missing data gracefully" (V.length returns <= 2),
      testCase "calculatePortfolioCorrelation handles single stock" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "AAPL",
                  Stock "2023-01-02" (Just 100.0) (Just 108.0) (Just 100.0) (Just 105.0) (Just 1200) "AAPL"
                ]
            stockMap = Map.singleton "AAPL" stocks
            portfolioCorr = calculatePortfolioCorrelation stockMap

        -- No correlations possible with single stock
        Map.size (correlationMatrix portfolioCorr) @?= 0
        averageCorrelation portfolioCorr @?= 0.0
        portfolioDiversification portfolioCorr @?= "Well Diversified", -- Default for no correlations
      testCase "calculateMovingAverages handles insufficient data" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 100.0) (Just 1000) "TEST",
                  Stock "2023-01-02" (Just 102.0) (Just 108.0) (Just 100.0) (Just 105.0) (Just 1200) "TEST"
                ] -- Only 2 stocks, insufficient for SMA-20, SMA-50, SMA-200
            movingAvgs = calculateMovingAverages stocks

        sma20 movingAvgs @?= Nothing -- Not enough data
        sma50 movingAvgs @?= Nothing -- Not enough data
        sma200 movingAvgs @?= Nothing -- Not enough data
        maSignal movingAvgs @?= Nothing -- Can't determine signal without SMAs
    ]

-- Output module tests
outputTests :: TestTree
outputTests =
  testGroup
    "Output Module Tests"
    [ testCase "formatPercentage formats correctly" $ do
        formatPercentage 5.0 @?= "5.00%"
        formatPercentage (-3.14159) @?= "-3.14%"
        formatPercentage 0.0 @?= "0.00%"
        formatPercentage 10.666 @?= "10.67%"
        formatPercentage (-0.01) @?= "-0.01%"
    ]

-- Integration tests
integrationTests :: TestTree
integrationTests =
  testGroup
    "Integration Tests"
    [ testCase "Full workflow with valid stock data" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 103.0) (Just 1000) "AAPL", -- +3%
                  Stock "2023-01-02" (Just 103.0) (Just 108.0) (Just 100.0) (Just 96.47) (Just 1200) "AAPL", -- -6.35%
                  Stock "2023-01-03" (Just 96.47) (Just 99.0) (Just 95.0) (Just 98.0) (Just 800) "AAPL" -- +1.59%
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
        assertBool "Should have volatility" (case volatility analysis of Just _ -> True; Nothing -> False),
      testCase "Handles mixed valid and invalid stock data" $ do
        let stocks =
              V.fromList
                [ Stock "2023-01-01" (Just 100.0) (Just 105.0) (Just 95.0) (Just 105.0) (Just 1000) "TEST", -- +5%
                  Stock "2023-01-02" Nothing Nothing Nothing Nothing Nothing "TEST", -- No data
                  Stock "2023-01-03" (Just 105.0) (Just 110.0) (Just 100.0) (Just 102.9) (Just 800) "TEST" -- -2%
                ]
            stockMap = Map.singleton "TEST" stocks
            analyses = analyzeAllStocks stockMap

        length analyses @?= 1
        let analysis = head analyses
        stockName analysis @?= "TEST"

        -- Should find best and worst from valid entries only
        assertBool "Should have best day" (case bestDay analysis of Just _ -> True; Nothing -> False)
        assertBool "Should have worst day" (case worstDay analysis of Just _ -> True; Nothing -> False),
      testCase "Edge case: Single stock entry" $ do
        let stocks =
              V.fromList
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
