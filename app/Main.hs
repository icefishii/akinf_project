module Main where

import qualified Data.Vector as V
import AkinfProject.CSV (loadStocks, Stock)

main :: IO ()
main = do
    result <- loadStocks "all_stocks_5yr.csv"
    case result of
        Left err -> putStrLn ("CSV parse error: " ++ err)
        Right stocks -> mapM_ print (V.take 5 stocks)
