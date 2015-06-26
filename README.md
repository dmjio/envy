envy
===================
Let's face it, dealing with environment variables in Haskell isn't that satisfying.

```haskell
import System.Environment
import Data.Text (pack)

data PGConfig = PGConfig {
  pgPort :: Int
  pgURL  :: Text
} deriving (Show, Eq)

getPGPort :: IO PGConfig
getPGPort = do
  portResult <- lookupEnv "PG_PORT"
  urlResult  <- lookupEnv "PG_URL"
  case (portResult, urlResult) of
    (Just port, Just url) ->
      case readMaybe port :: Maybe Int of
        Nothing -> error "PG_PORT isn't a number"
        Just portNum -> return $ PGConfig portNum (T.pack y)
    (Nothing, _) -> error "Couldn't find PG_PORT"    
    (_, Nothing) -> error "Couldn't find PG_URL"    
    -- Pretty gross right...
```

What if we could apply Aeson's FromJSON / ToJSON pattern to solve this problem?

```haskell
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
-- | PGConfig -- hyopthetical postgresql config
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
```
