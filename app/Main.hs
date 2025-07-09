module Main where

import qualified Data.Vector as V
import AkinfProject.CSV (loadStocks)
import AkinfProject.Config (loadConfig)  -- import from Config now

main :: IO ()
main = do
    configResult <- loadConfig "config.yaml"  -- YAML config filename
    case configResult of
      Left err -> putStrLn ("Config parse error: " ++ show err)  -- show error
      Right config -> do
        putStrLn "Loaded config:"
        print config

        stockResult <- loadStocks "all_stocks_5yr.csv"
        case stockResult of
          Left err -> putStrLn ("CSV parse error: " ++ err)
          Right stocks -> do
            putStrLn "\nFirst 5 stocks:"
            mapM_ print (V.take 5 stocks)
