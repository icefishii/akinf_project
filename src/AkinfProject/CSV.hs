{-# LANGUAGE OverloadedStrings #-}

module AkinfProject.CSV
  ( Stock(..)
  , loadStocks
  , parseStocksFromBytes
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (decodeByName, FromNamedRecord, (.:))
import qualified Data.Csv as Csv
import GHC.Generics (Generic)

-- | Data type for a stock record.
data Stock = Stock
    { date   :: !String
    , open   :: !(Maybe Double)
    , high   :: !(Maybe Double)
    , low    :: !(Maybe Double)
    , close  :: !(Maybe Double)
    , volume :: !(Maybe Int)
    , name   :: !String
    } deriving (Show, Generic)

-- | How to parse CSV rows into Stock records.
instance FromNamedRecord Stock where
    parseNamedRecord m = Stock
        <$> m .: "date"
        <*> m .: "open"
        <*> m .: "high"
        <*> m .: "low"
        <*> m .: "close"
        <*> m .: "volume"
        <*> m .: "Name"

-- | Load all stocks from a CSV file.
loadStocks :: FilePath -> IO (Either String (V.Vector Stock))
loadStocks path = do
    csvData <- BL.readFile path
    return $ parseStocksFromBytes csvData

-- | Parse CSV data from a lazy ByteString directly (used for testing).
parseStocksFromBytes :: BL.ByteString -> Either String (V.Vector Stock)
parseStocksFromBytes bs =
    case decodeByName bs of
        Left err     -> Left err
        Right (_, v) -> Right v
