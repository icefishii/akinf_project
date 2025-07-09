module Main where

import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import AkinfProject.CSV (loadStocks, Stock(..))
import AkinfProject.Config (loadConfig)
import AkinfProject.Filter (filterByConfig)

main :: IO ()
main = do
    configResult <- loadConfig "config.yaml"
    case configResult of
      Left err -> putStrLn ("Config parse error: " ++ show err)
      Right config -> do
        putStrLn "Loaded config:"
        print config

        stockResult <- loadStocks "all_stocks_5yr.csv"
        case stockResult of
          Left err -> putStrLn ("CSV parse error: " ++ err)
          Right stocks -> do
            putStrLn "\nLoaded stock names (first 10):"
            print (take 10 . V.toList $ V.map name stocks)

            let filtered = filterByConfig config stocks

            putStrLn "\nFiltered results:"
            mapM_ (\(k, v) -> putStrLn ("Stock: " ++ k ++ ", entries: " ++ show (V.length v)))
                  (Map.toList filtered)

            putStrLn $ "Total stocks after filtering: " ++ show (sum (map V.length (Map.elems filtered)))
            putStrLn "Done processing stocks."
