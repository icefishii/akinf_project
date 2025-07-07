{-# LANGUAGE OverloadedStrings #-}

module Main where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (decodeByName, FromNamedRecord, (.:))
import qualified Data.Csv as Csv
import Control.Monad (forM_)
import GHC.Generics (Generic)

data Stock = Stock
    { date   :: !String
    , open   :: !(Maybe Double)
    , high   :: !(Maybe Double)
    , low    :: !(Maybe Double)
    , close  :: !(Maybe Double)
    , volume :: !(Maybe Int)
    , name   :: !String
    } deriving (Show, Generic)

instance FromNamedRecord Stock where
    parseNamedRecord m = Stock
        <$> m .: "date"
        <*> m .: "open"
        <*> m .: "high"
        <*> m .: "low"
        <*> m .: "close"
        <*> m .: "volume"
        <*> m .: "Name"

main :: IO ()
main = do
    csvData <- BL.readFile "all_stocks_5yr.csv"
    case decodeByName csvData of
        Left err -> putStrLn ("CSV parse error: " ++ err)
        Right (_, v) ->
            forM_ (V.take 5 (v :: V.Vector Stock)) print