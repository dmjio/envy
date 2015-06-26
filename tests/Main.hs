{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
------------------------------------------------------------------------------
module Main ( main ) where
------------------------------------------------------------------------------
import System.Environment
import Control.Monad
import System.Envy
import Test.Hspec
import Data.Text    (Text)
import Data.Either
------------------------------------------------------------------------------
-- | PGConfig
data PGConfig = PGConfig {
    pgPort :: Text -- ^ Port 
  , pgURL  :: Int  -- ^ URL
  } deriving (Show, Read, Eq)

------------------------------------------------------------------------------
-- | Instances
instance FromEnv PGConfig where
  fromEnv env = do
    PGConfig <$> "PG_PORT" .: env 
             <*> "PG_URL"  .: env

------------------------------------------------------------------------------
-- | To Environment Instances
instance ToEnv PGConfig where
  toEnv PGConfig{..} =
       [ "PG_PORT" .= pgPort
       , "PG_URL"  .= pgURL
       ]

------------------------------------------------------------------------------
-- | Start tests
main :: IO ()
main = do 
  let pgConfig = PGConfig 2345 "localhost"
  setEnvironment pgConfig
  print =<< do parseEnv :: IO (Either String PGConfig)
  -- unsetEnvironment pgConfig -- remove when done

