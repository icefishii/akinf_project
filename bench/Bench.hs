{-# LANGUAGE OverloadedStrings #-}

module Main where

import AkinfProject.CSV (parseStocksFromBytes)
import AkinfProject.Calculate (analyzePortfolio)
import AkinfProject.Config (loadConfig)
import AkinfProject.Filter (filterByConfig)
import Criterion.Main
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
  -- Read both config and CSV files
  configResult <- loadConfig "config.yaml"
  csvData <- BL.readFile "all_stocks_5yr.csv"

  -- Parse CSV data once to use for multiple benchmarks
  let parsedStocks = parseStocksFromBytes csvData

  case (configResult, parsedStocks) of
    (Left err, _) -> putStrLn $ "Error parsing config for benchmarks: " ++ show err
    (_, Left err) -> putStrLn $ "Error parsing CSV for benchmarks: " ++ err
    (Right config, Right allStocks) -> do
      defaultMain
        [ bgroup
            "Parsing benchmarks"
            [ bench "parse csv" $ whnf parseStocksFromBytes csvData,
              bench "load config" $ nfIO (fmap show (loadConfig "config.yaml"))
            ],
          bgroup
            "Processing benchmarks"
            [ bench "filter by config" $ whnf (filterByConfig config) allStocks
            ],
          bgroup
            "Analysis benchmarks"
            [ let filteredStocks = filterByConfig config allStocks
                  stocksMapString = Map.mapKeys show filteredStocks
               in bench "analyzePortfolio" $ whnf analyzePortfolio stocksMapString
            ]
        ]
