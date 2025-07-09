{-# LANGUAGE OverloadedStrings #-}

module Main where

import AkinfProject.CSV (parseStocksFromBytes)
import AkinfProject.Config (Config)
import Criterion.Main
import Data.Bifunctor qualified
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
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

  defaultMain
    [ bgroup
        "Parsing benchmarks"
        [ bench "parse config" $ whnf parseConfig configData,
          bench "parse csv" $ whnf parseStocksFromBytes csvData,
          bench "parse config + csv" $ whnf (Data.Bifunctor.bimap parseConfig parseStocksFromBytes) (configData, csvData)
        ]
    ]
