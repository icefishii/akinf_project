{-# LANGUAGE DeriveGeneric #-}
module AkinfProject.Config where

import Data.Yaml
import GHC.Generics (Generic)

data Config = Config
  { stocks    :: [String]
  , timeframe :: Timeframe
  } deriving (Show, Generic)

data Timeframe = Timeframe
  { start_date :: String
  , end_date   :: String
  } deriving (Show, Generic)

instance FromJSON Config
instance FromJSON Timeframe

loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig = decodeFileEither
