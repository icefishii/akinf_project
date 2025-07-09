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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
  [ csvTests
  , configTests
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
