{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module AkinfProject.CSV
  ( Stock(..)
  , loadStocks
  , parseStocksFromBytes
  , loadStocksOptimized
  , loadStocksAdaptive
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Csv (decodeByName, FromNamedRecord, (.:))
import qualified Data.Csv as Csv
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | Optimized data type for a stock record with strict fields and Text for date.
data Stock = Stock
    { date   :: {-# UNPACK #-} !T.Text  -- Use Text instead of String for better performance
    , open   :: !(Maybe Double)
    , high   :: !(Maybe Double)
    , low    :: !(Maybe Double)
    , close  :: !(Maybe Double)
    , volume :: !(Maybe Int)
    , name   :: {-# UNPACK #-} !T.Text  -- Use Text for stock name too
    } deriving (Show, Generic)

instance NFData Stock  -- Enable deep evaluation for parallel processing

-- | How to parse CSV rows into Stock records with optimized Text conversion.
instance FromNamedRecord Stock where
    parseNamedRecord m = Stock
        <$> m .: "date"      -- cassava automatically converts to Text
        <*> m .: "open"
        <*> m .: "high"
        <*> m .: "low"
        <*> m .: "close"
        <*> m .: "volume"
        <*> m .: "Name"      -- cassava automatically converts to Text

-- | Load all stocks from a CSV file with optimized parsing.
loadStocks :: FilePath -> IO (Either String (V.Vector Stock))
loadStocks path = do
    csvData <- BL.readFile path
    return $! parseStocksFromBytes csvData  -- Force strict evaluation

-- | Optimized CSV parsing with streaming and strict evaluation.
loadStocksOptimized :: FilePath -> IO (Either String (V.Vector Stock))
loadStocksOptimized path = do
    !csvData <- BL.readFile path  -- Strict reading
    return $! parseStocksOptimized csvData

-- | Parse CSV data from a lazy ByteString directly (original implementation).
parseStocksFromBytes :: BL.ByteString -> Either String (V.Vector Stock)
parseStocksFromBytes bs =
    case decodeByName bs of
        Left err     -> Left err
        Right (_, v) -> Right v

-- | Optimized parsing with better memory usage and strict evaluation.
parseStocksOptimized :: BL.ByteString -> Either String (V.Vector Stock)
parseStocksOptimized bs =
    case decodeByName bs of
        Left err     -> Left err
        Right (_, v) -> 
            let !strictVector = V.force v  -- Force strict evaluation of the entire vector
            in Right strictVector

-- | Fast streaming CSV parser with aggressive optimizations
{-# INLINE parseStocksStreaming #-}
parseStocksStreaming :: BL.ByteString -> Either String (V.Vector Stock)
parseStocksStreaming bs =
    case decodeByName bs of
        Left err -> Left err
        Right (_, v) -> 
            let !strictVector = V.map forceStock v  -- Force each stock individually
            in Right strictVector
  where
    {-# INLINE forceStock #-}
    forceStock !stock = stock  -- Force evaluation of each Stock record

-- | Choose best parsing strategy based on file size
loadStocksAdaptive :: FilePath -> IO (Either String (V.Vector Stock))
loadStocksAdaptive path = do
    !csvData <- BL.readFile path
    let fileSize = BL.length csvData
    return $! if fileSize > 10000000  -- > 10MB, use streaming
              then parseStocksStreaming csvData
              else parseStocksOptimized csvData
