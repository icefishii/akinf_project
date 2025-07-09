module Main where

import AkinfProject.CSV (Stock (..), loadStocksAdaptive)
import AkinfProject.Calculate (analyzePortfolio)
import AkinfProject.Config (loadConfig)
import AkinfProject.Filter (filterByConfig)
import AkinfProject.Output (displayAnalysis, displayPortfolioCorrelation, displaySummary)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V

main :: IO ()
main = do
  configResult <- loadConfig "config.yaml"
  -- loadConfig returns an Either Monad so Left & Right is used for Error Handling
  case configResult of
    Left err -> putStrLn ("Config parse error: " ++ show err)
    Right config -> do
      putStrLn "Loaded config:"
      -- Config derives from Show so it can be printed
      print config

      stockResult <- loadStocksAdaptive "all_stocks_5yr.csv"
      case stockResult of
        Left err -> putStrLn ("CSV parse error: " ++ err)
        Right stocks -> do
          putStrLn "\nLoaded stock names (first 10):"
          print (take 10 . V.toList $ V.map name stocks)

          let filtered = filterByConfig config stocks

          putStrLn "\nFiltered results:"
          mapM_
            (\(k, v) -> putStrLn ("Stock: " ++ k ++ ", entries: " ++ show (V.length v)))
            (Map.toList filtered)

          putStrLn $ "Total stocks after filtering: " ++ show (sum (map V.length (Map.elems filtered)))

          -- Perform comprehensive analysis including correlations
          let (analyses, portfolioCorr) = analyzePortfolio filtered
          displayAnalysis analyses
          displaySummary analyses
          displayPortfolioCorrelation portfolioCorr

          putStrLn "Done processing stocks."
