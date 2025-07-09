{-# LANGUAGE OverloadedStrings #-}

module Main where

import AkinfProject.CSV (Stock (..), parseStocksFromBytes)
-- Added for completeness, though less computationally intensive

import AkinfProject.Calculate (analyzeAllStocks, analyzePortfolio, analyzeStock, calculateCorrelation, calculateDailyReturns, calculateMovingAverages, calculatePortfolioCorrelation, calculateROI, calculateRiskLevel, calculateSMA, dailyVolatility, detectTrend, determineMASignal, percentageChange)
import AkinfProject.Config (Config)
import AkinfProject.Output (formatPercentage)
import Criterion.Main
import Data.Bifunctor qualified
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Yaml (decodeEither')

-- parseConfig from bytestring using Data.Yaml.decodeEither'
parseConfig :: BS.ByteString -> Either String Config
parseConfig bs = case decodeEither' bs of
  Left err -> Left (show err)
  Right cfg -> Right cfg

main :: IO ()
main = do
  -- Read config as strict ByteString for Data.Yaml
  configData <- BS.readFile "config.yaml"
  -- Read CSV as lazy ByteString for cassava
  csvData <- BL.readFile "all_stocks_5yr.csv"

  -- Parse CSV data once to use for multiple benchmarks
  let parsedStocksE = parseStocksFromBytes csvData

  case parsedStocksE of
    Left err -> putStrLn $ "Error parsing CSV for benchmarks: " ++ err
    Right allStocks -> do
      -- Group stocks by name for functions requiring Map StockName (Vector Stock)
      let stocksMap = V.foldl' (\acc stock -> Map.insertWith (V.++) (name stock) (V.singleton stock) acc) Map.empty allStocks

      defaultMain
        [ bgroup
            "Parsing benchmarks"
            [ bench "parse config" $ whnf parseConfig configData,
              bench "parse csv" $ whnf parseStocksFromBytes csvData,
              bench "parse config + csv" $ whnf (Data.Bifunctor.bimap parseConfig parseStocksFromBytes) (configData, csvData)
            ],
          -- Benchmarks for Calculate module functions
          bgroup
            "Calculate module benchmarks"
            [ let aaplStocks = Map.findWithDefault V.empty "AAPL" stocksMap
               in bgroup
                    "Single stock analysis (AAPL)"
                    [ bench "percentageChange" $ whnf (V.map percentageChange) aaplStocks,
                      bench "dailyVolatility" $ whnf (V.map dailyVolatility) aaplStocks,
                      bench "calculateROI" $ whnf calculateROI aaplStocks,
                      bench "detectTrend" $ whnf detectTrend aaplStocks,
                      bench "calculateRiskLevel" $ whnf calculateRiskLevel aaplStocks,
                      bench "calculateSMA 20" $ whnf (calculateSMA 20) aaplStocks,
                      bench "calculateSMA 50" $ whnf (calculateSMA 50) aaplStocks,
                      bench "calculateSMA 200" $ whnf (calculateSMA 200) aaplStocks,
                      bench "calculateMovingAverages" $ whnf calculateMovingAverages aaplStocks,
                      -- determineMASignal takes individual SMAs, so we'll pass those in directly
                      -- We assume these values are representative, or you can pre-calculate them
                      bench "determineMASignal" $ whnf (determineMASignal (Just 110.0) (Just 105.0)) (Just 100.0),
                      bench "analyzeStock" $ whnf (analyzeStock "AAPL") aaplStocks
                    ],
              bench "analyzeAllStocks" $ whnf analyzeAllStocks (Map.mapKeys show stocksMap),
              -- Benchmarks for portfolio functions
              bgroup
                "Portfolio analysis"
                [ bench "calculateDailyReturns (AAPL)" $ whnf calculateDailyReturns (Map.findWithDefault V.empty "AAPL" stocksMap),
                  bench "calculateDailyReturns (MSFT)" $ whnf calculateDailyReturns (Map.findWithDefault V.empty "MSFT" stocksMap),
                  -- For calculateCorrelation, you need two sets of returns.
                  -- We'll extract them here for the benchmark.
                  let aaplReturns = calculateDailyReturns (Map.findWithDefault V.empty "AAPL" stocksMap)
                      msftReturns = calculateDailyReturns (Map.findWithDefault V.empty "MSFT" stocksMap)
                   in bench "calculateCorrelation (AAPL, MSFT)" $ whnf (calculateCorrelation aaplReturns) msftReturns,
                  let stocksMapString = Map.mapKeys show stocksMap
                   in bench "calculatePortfolioCorrelation" $ whnf calculatePortfolioCorrelation stocksMapString,
                  let stocksMapString = Map.mapKeys show stocksMap
                   in bench "analyzePortfolio" $ whnf analyzePortfolio stocksMapString
                ]
            ],
          -- Benchmarks for Output module functions
          bgroup
            "Output module benchmarks"
            [ bench "formatPercentage" $ whnf formatPercentage 5.6789
            ]
        ]
