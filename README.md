envy [![Circle CI](https://circleci.com/gh/dmjio/envy.svg?style=svg)](https://circleci.com/gh/dmjio/envy) [![Hackage](https://img.shields.io/hackage/v/envy.svg?style=flat)](https://hackage.haskell.org/package/envy)
===================
Let's face it, dealing with environment variables in Haskell isn't that satisfying.

```haskell
import System.Environment
import Data.Text (pack)
import Text.Read (readMaybe)

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
        Just portNum -> return $ PGConfig portNum (pack url)
    (Nothing, _) -> error "Couldn't find PG_PORT"    
    (_, Nothing) -> error "Couldn't find PG_URL"    
    -- Pretty gross right...
```

Another attempt to remedy the lookup madness is with a `MaybeT IO a`. See below.
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.Environment

newtype Env a = Env { unEnv :: MaybeT IO a }
    deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

getEnv :: Env a -> IO (Maybe a)
getEnv env = runMaybeT (unEnv env)

env :: String -> Env a
env key = Env (MaybeT (lookupEnv key))

connectInfo :: Env ConnectInfo
connectInfo = ConnectInfo
   <$> env "PG_HOST"
   <*> env "PG_PORT"
   <*> env "PG_USER"
   <*> env "PG_PASS"
   <*> env "PG_DB"
```
This abstraction falls short in two areas
  - 1) Environment lookups don't return any information when a variable doesn't exist (just a `Nothing`)
  - 2) Environment lookups don't attempt to parse the returned type into something meaningful (everything is returned as a `String`)

What if we could apply aeson's `FromJSON` / `ToJSON` pattern to provide variable lookups that provide both key-lookup and parse failure information?
Armed with the `GeneralizedNewTypeDeriving` extension we can derive instances of `Var` that will parse to and from an environment. The `Var` typeclass is simply:
```haskell
class (Read a, Show a) => Var a where
  toVar   :: a -> String
  fromVar :: String -> Maybe a
```
With instances for most primitive types supported (`Word8` - `Word64`, `Int`, `Integer`, `String`, `Text`, etc.) the `Var` class is easily deriveable. The `FromEnv` typeclass provides a parser type that is an instance of `MonadError String` and `MonadIO`. This allows for connection pool initialization inside of our environment parser and custom error handling. The `ToEnv` class allows us to create an environment configuration given any `a`. See below for an example.

```haskell
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
------------------------------------------------------------------------------
module Main ( main ) where
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Either
import           Data.Word
import           System.Environment
import           System.Envy
------------------------------------------------------------------------------
data ConnectInfo = ConnectInfo {
      pgHost :: String
    , pgPort :: Word16
    , pgUser :: String
    , pgPass :: String
    , pgDB   :: String
  } deriving (Show)

------------------------------------------------------------------------------
-- | Posgtres config
data PGConfig = PGConfig {
    pgConnectInfo :: ConnectInfo -- ^ Connnection Info
  } 

------------------------------------------------------------------------------
-- | Custom show instance
instance Show PGConfig where
  show PGConfig {..} = "<PGConfig>"

------------------------------------------------------------------------------
-- | FromEnv instances support popular aeson combinators *and* IO
-- for dealing with connection pools. `env` is equivalent to (.:) in `aeson`
-- and `envMaybe` is equivalent to (.:?), except here the lookups are impure.
instance FromEnv PGConfig where
  fromEnv = PGConfig <$> (ConnectInfo <$> envMaybe "PG_HOST" .!= "localhost"
                                      <*> env "PG_PORT"
                                      <*> env "PG_USER" 
                                      <*> env "PG_PASS" 
                                      <*> env "PG_DB")

------------------------------------------------------------------------------
-- | To Environment Instances
-- (.=) is a smart constructor for producing types of `EnvVar` (which ensures
-- that Strings are set properly in an environment so they can be parsed properly
instance ToEnv PGConfig where
  toEnv = makeEnv 
       [ "PG_HOST" .= ("localhost" :: String)
       , "PG_PORT" .= (5432        :: Word16)
       , "PG_USER" .= ("user"      :: String)
       , "PG_PASS" .= ("pass"      :: String)
       , "PG_DB"   .= ("db"        :: String)
       ]

------------------------------------------------------------------------------
-- | Example
main :: IO ()
main = do
   setEnvironment (toEnv :: EnvList PGConfig)
   result <- decodeEnv :: IO (Either String PGConfig)
   print result -- "Right <PGConfig>"
   -- unsetEnvironment (toEnv :: EnvList PGConfig)  -- remove when done
```

*Note*: As of base 4.7 `setEnv` and `getEnv` throw an `IOException` if a `=` is present in an environment. `envy` catches these synchronous exceptions and delivers them
purely to the end user.
