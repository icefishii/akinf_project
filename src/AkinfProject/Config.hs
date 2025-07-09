{-# LANGUAGE DeriveGeneric #-}

module AkinfProject.Config where

import Data.Yaml
import GHC.Generics (Generic)

-- Create Structs which "inherit" from Show (for printing)
-- and Generic (for being able to set instance of)
data Config = Config
  { stocks :: [String],
    timeframe :: Timeframe
  }
  deriving (Show, Generic)

data Timeframe = Timeframe
  { start_date :: String,
    end_date :: String
  }
  deriving (Show, Generic)

-- Making Config and TimeFram instances of the FromJson Type
instance FromJSON Config

instance FromJSON Timeframe

-- Typesignature to make sure the returned type is Config
loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig = decodeFileEither
