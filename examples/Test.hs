{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Envy
import GHC.Generics

data PGConfig = PGConfig {
    connectHost :: String -- "PG_HOST"
  , connectPort :: Int    -- "PG_PORT"
  } deriving (Generic, Show)

instance DefConfig PGConfig where
  defConfig = PGConfig "localhost" 5432

instance FromEnv PGConfig where
  fromEnv = gFromEnvCustom (Option 7 "PG")
  -- Generically creates instance for retrieving environment variables (PG_HOST, PG_PORT)

main :: IO ()
main =
  print =<< (decodeEnv :: IO (Either String PGConfig))
 -- PGConfig { pgHost = "foobah", pgPort = 5432 }
