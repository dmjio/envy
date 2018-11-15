envy
===================
[![Hackage](https://img.shields.io/hackage/v/envy.svg)](https://hackage.haskell.org/package/envy)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/envy.svg)](https://packdeps.haskellers.com/feed?needle=envy)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
[![Build Status](https://travis-ci.org/dmjio/envy.svg?branch=master)](https://travis-ci.org/dmjio/envy)

Let's face it, dealing with environment variables in Haskell isn't that satisfying.

```haskell
import System.Environment
import Data.Text (pack)
import Text.Read (readMaybe)

data ConnectInfo = ConnectInfo {
  pgPort :: Int
  pgURL  :: Text
} deriving (Show, Eq)

getPGPort :: IO ConnectInfo
getPGPort = do
  portResult <- lookupEnv "PG_PORT"
  urlResult  <- lookupEnv "PG_URL"
  case (portResult, urlResult) of
    (Just port, Just url) ->
      case readMaybe port :: Maybe Int of
	Nothing -> error "PG_PORT isn't a number"
	Just portNum -> return $ ConnectInfo portNum (pack url)
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
This abstraction falls short in two areas:
  - Lookups don't return any information when a variable doesn't exist (just a `Nothing`)
  - Lookups don't attempt to parse the returned type into something meaningful (everything is returned as a `String` because `lookupEnv :: String -> IO (Maybe String)`)

What if we could apply aeson's `FromJSON` / `ToJSON` pattern to give us variable lookups that provide both key-lookup and parse failure information?
Armed with the `GeneralizedNewTypeDeriving` extension we can derive instances of `Var` that will parse to and from an environment variable. The `Var` typeclass is simply:
```haskell
class Var a where
  toVar   :: a -> String
  fromVar :: String -> Maybe a
```
With instances for most concrete and primitive types supported (`Word8` - `Word64`, `Int`, `Integer`, `String`, `Text`, etc.) the `Var` class is easily deriveable. The `FromEnv` typeclass provides a parser type that is an instance of `MonadError String` and `MonadIO`. This allows for connection pool initialization inside of our environment parser and custom error handling. The `ToEnv` class allows us to create an environment configuration given any `a`. See below for an example.

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
-- | FromEnv instances support popular aeson combinators *and* IO
-- for dealing with connection pool initialization. `env` is equivalent to (.:) in `aeson`
-- and `envMaybe` is equivalent to (.:?), except here the lookups are impure.
instance FromEnv ConnectInfo where
  fromEnv =
    ConnectInfo <$> envMaybe "PG_HOST" .!= "localhost"
		<*> env "PG_PORT"
		<*> env "PG_USER"
		<*> env "PG_PASS"
		<*> env "PG_DB"

------------------------------------------------------------------------------
-- | To Environment Instances
-- (.=) is a smart constructor for producing types of `EnvVar` (which ensures
-- that Strings are set properly in an environment so they can be parsed properly
instance ToEnv ConnectInfo where
  toEnv ConnectInfo {..} = makeEnv
       [ "PG_HOST" .= pgHost
       , "PG_PORT" .= pgPort
       , "PG_USER" .= pgUser
       , "PG_PASS" .= pgPass
       , "PG_DB"   .= pgDB
       ]

------------------------------------------------------------------------------
-- | Example
main :: IO ()
main = do
   setEnvironment (toEnv :: EnvList ConnectInfo)
   print =<< do decodeEnv :: IO (Either String ConnectInfo)
   -- unsetEnvironment (toEnv :: EnvList ConnectInfo)  -- remove when done
```

*Note*: As of base 4.7 `setEnv` and `getEnv` throw an `IOException` if a `=` is present in an environment. `envy` catches these synchronous exceptions and delivers them
purely to the end user.

Generics
===================

As of version `1.0`, all `FromEnv` instance boilerplate can be completely removed thanks to `GHC.Generics`! Below is an example.

```haskell
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Envy
import GHC.Generics

-- This record corresponds to our environment, where the field names become the variable names, and the values the environment variable value
data PGConfig = PGConfig {
    pgHost :: String -- "PG_HOST"
  , pgPort :: Int    -- "PG_PORT"
  } deriving (Generic, Show)

-- Default configuration will be used for fields that could not be retrieved from the environment
instance DefConfig PGConfig where
  defConfig = PGConfig "localhost" 5432

instance FromEnv PGConfig
-- Generically creates instance for retrieving environment variables (PG_HOST, PG_PORT)

main :: IO ()
main =
  print =<< decodeEnv :: IO (Either String PGConfig)
 -- > PGConfig { pgHost = "customURL", pgPort = 5432 }
```

Suppose you'd like to customize the field name (i.e. add your own prefix, or drop the existing record prefix). This too is possible. See below.

```haskell
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

-- All fields will be converted to uppercase
instance FromEnv PGConfig where
  fromEnv = fromEnvCustom Option {
                    dropPrefixCount = 7
                  , customPrefix = "PG"
		  }

main :: IO ()
main =
  print =<< decodeEnv :: IO (Either String PGConfig)
 -- PGConfig { pgHost = "customUrl", pgPort = 5432 }
```

It's also possible to avoid typeclasses altogether using `runEnv` with `gFromEnvCustom`.

```haskell
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Envy
import GHC.Generics

data PGConfig = PGConfig {
    connectHost :: String -- "PG_HOST"
  , connectPort :: Int    -- "PG_PORT"
  } deriving (Generic, Show)

-- Default PGConfig
instance DefConfig PGConfig where
  defConfig = PGConfig "localhost" 5432

-- All fields will be converted to uppercase
getPGEnv :: IO (Either String PGConfig)
getPGEnv = runEnv $ gFromEnvCustom Option {
                    dropPrefixCount = 7
                  , customPrefix = "PG"
		  }

main :: IO ()
main = print =<< getPGEnv
 -- PGConfig { pgHost = "customUrl", pgPort = 5432 }
```
