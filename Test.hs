{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Envy
import GHC.Generics

data PGConfig = PGConfig {
    pgHost :: String -- "PG_HOST"
  , pgPort :: Int    -- "PG_PORT"
  } deriving (Generic, Show)

instance FromEnv PGConfig

instance DefConfig PGConfig where
  defConfig = PGConfig "localhost" 5432

main :: IO ()
main = print =<< do decodeEnv :: IO (Either String PGConfig)

