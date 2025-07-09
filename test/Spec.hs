{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import AkinfProject.CSV (parseStocksFromBytes, Stock(..))

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CSV Parsing Tests"
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
