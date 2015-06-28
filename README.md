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
What if we could apply aeson's `FromJSON` / `ToJSON` pattern to provide a cleaner interface? Armed with the `GeneralizedNewTypeDeriving` extension we can derive instances of `Var` that will parse to and from an environment. The `Var` typeclass is simply:
```haskell
class (Read a, Show a) => Var a where
  toVar   :: a -> String
  fromVar :: String -> Maybe a
```
With instances for most primitive types supported (`Word8` - `Word64`, `Int`, `Integer`, `String`, `Text`, etc.) the `Var` class is easily deriveable. The `FromEnv` typeclass provides a parser type that is an instance of `MonadReader Env`, `MonadError String` and `MonadIO`. This allows for connection pool initialization inside of our environment parser, custom error handling and environment fetching. See below for an example.

```haskell
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
------------------------------------------------------------------------------
module Main ( main ) where
------------------------------------------------------------------------------
import System.Environment
import Control.Monad
import System.Envy
import Data.Either
import Data.Word
import Data.String
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error
import Data.Typeable
import Data.Int
import Data.Text    (Text)
import Database.PostgreSQL.Simple
------------------------------------------------------------------------------
-- | Posgtres Port
newtype PGPORT = PGPORT Word16
     deriving (Read, Show, Var, Typeable, Num)

------------------------------------------------------------------------------
-- | Postgres URL
newtype PGURL = PGURL String
     deriving (Read, Show, Var, IsString, Typeable)

------------------------------------------------------------------------------
-- | Postgres Host
newtype PGHOST = PGHOST String
     deriving (Read, Show, Var, IsString, Typeable)

------------------------------------------------------------------------------
-- | Postgres DB
newtype PGDB = PGDB String
     deriving (Read, Show, Var, IsString, Typeable)

------------------------------------------------------------------------------
-- | Postgres User
newtype PGUSER = PGUSER String
     deriving (Read, Show, Var, IsString, Typeable)

------------------------------------------------------------------------------
-- | Postgres Password
newtype PGPASS = PGPASS String
     deriving (Read, Show, Var, IsString, Typeable)

------------------------------------------------------------------------------
-- | Posgtres config
data PGConfig = PGConfig {
    pgConnectInfo :: ConnectInfo -- ^ Connnection Info
  , pgConnection  :: Connection  -- ^ Connection Pool 
  } 

------------------------------------------------------------------------------
-- | Custom show instance
instance Show PGConfig where
  show PGConfig {..} = "<PGConfig>"

------------------------------------------------------------------------------
-- | FromEnv Instances, supports popular aeson combinators *and* IO
-- for dealing with connection pools
instance FromEnv PGConfig where
  fromEnv env = do
    conInfo <- ConnectInfo <$> "PG_HOST" .:? env .!= ("localhost" :: String)
                           <*> "PG_PORT" .: env 
                           <*> "PG_USER" .: env 
                           <*> "PG_PASS" .: env 
                           <*> "PG_DB"   .: env 
    result <- liftIO $ try (connect conInfo)
    case result of
      Left (err :: IOException) -> throwError (show err)
      Right con -> return $ PGConfig conInfo con

------------------------------------------------------------------------------
-- | To Environment Instances
instance ToEnv PGConfig where
  toEnv = makeEnv 
       [ "PG_HOST" .= PGHOST "localhost"
       , "PG_PORT" .= PGPORT 5432
       , "PG_USER" .= PGUSER "user"
       , "PG_PASS" .= PGPASS "pass"
       , "PG_DB"   .= PGDB "db"
       ]

------------------------------------------------------------------------------
-- | Example
main :: IO ()
main = do
   setEnvironment (toEnv :: EnvList PGConfig)
   result <- decodeEnv :: IO (Either String PGConfig)
   print result -- "Right <PGConfig>", connection pools initialized from environment set values
   -- unsetEnvironment (toEnv :: EnvList PGConfig)  -- remove when done
```

*Note*: As of base 4.7 `setEnv` and `getEnv` throw an `IOException` if a `=` is present in an environment. `envy` catches these synchronous exceptions and delivers them
purely to the end user.
