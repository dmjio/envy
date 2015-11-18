{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
------------------------------------------------------------------------------
-- |
-- Module      : System.Envy
-- Copyright   : (c) David Johnson 2015
-- Maintainer  : djohnson.m@ngmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > module Main ( main ) where
-- >
-- > import System.Envy
-- > import GHC.Generics
-- >
-- > data PGConfig = PGConfig {
-- >   pgHost :: String -- "PG_HOST"
-- > , pgPort :: Int    -- "PG_PORT"
-- > } deriving (Generic, Show)
-- >
-- > -- Default instance used if environment variable doesn't exist
-- > instance DefConfig PGConfig where
-- >   defConfig = PGConfig "localhost" 5432
-- >
-- > instance FromEnv PGConfig
-- > -- Generically produces the following body (no implementation needed if using Generics):
-- > -- fromEnv = PGConfig <$> envMaybe "PG_HOST" .!= "localhost"
-- > --                    <*> envMaybe "PG_PORT" .!= 5432
-- >
-- > main :: IO ()
-- > main =
-- >   print =<< do decodeEnv :: IO (Either String PGConfig)
-- >  -- PGConfig { pgHost = "custom-pg-url", pgPort = 5432 }
--
module System.Envy
       ( -- * Classes
         FromEnv (..)
       , ToEnv   (..)
       , Var     (..)
       , EnvList
        -- * Functions
       , decodeEnv
       , decode
       , showEnv
       , setEnvironment
       , setEnvironment'
       , unsetEnvironment
       , makeEnv
       , env
       , envMaybe
       , (.=)
       , (.!=)
         -- * Generics
       , DefConfig (..)
       , Option (..)
       ) where
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Except
import           Control.Exception
import           Data.Maybe
import           Data.Char
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           System.Environment
import           Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Word
import           Data.Int
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
------------------------------------------------------------------------------
-- | Parser Monad for environment variable retrieval
newtype Parser a = Parser { runParser :: ExceptT String IO a }
  deriving ( Functor, Monad, Applicative, MonadError String
           , MonadIO, Alternative, MonadPlus )

------------------------------------------------------------------------------
-- | Variable type, smart constructor for handling environment variables
data EnvVar = EnvVar { getEnvVar :: (String, String) }
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Executes `Parser`
evalParser :: FromEnv a => Parser a -> IO (Either String a)
evalParser = runExceptT . runParser

------------------------------------------------------------------------------
-- | Environment variable getter
getE
  :: forall a . (Typeable a, Var a)
  => String
  -> Parser a
getE k = do
  result <- liftIO (lookupEnv k)
  case result of
    Nothing -> throwError $ "Variable not found for: " ++ k
    Just dv ->
      case fromVar dv :: Maybe a of
        Nothing -> throwError $ "Parse failure: field <name> is not of type: "
                     ++ show (typeOf dv)
        Just x -> return x

------------------------------------------------------------------------------
-- | Environment variable getter
env :: forall a. (Typeable a, Var a)
    => String
    -> Parser a
env = getE

------------------------------------------------------------------------------
-- | Environment variable getter returning `Maybe`
getEMaybe
  :: forall a . (Typeable a, Var a)
  => String
  -> Parser (Maybe a)
getEMaybe k = do
  val <- liftIO (lookupEnv k)
  return $ case val of
   Nothing -> Nothing
   Just x -> fromVar x

------------------------------------------------------------------------------
-- | Environment variable getter returning `Maybe`
envMaybe :: forall a. (Typeable a, Var a)
  => String
  -> Parser (Maybe a)
envMaybe = getEMaybe

------------------------------------------------------------------------------
-- | For use with (.:?) for providing default arguments
(.!=) :: forall a. (Typeable a, Var a)
  => Parser (Maybe a)
  -> a
  -> Parser a
(.!=) p x  = fmap (fromMaybe x) p

------------------------------------------------------------------------------
-- | Infix environment variable setter
-- Smart constructor for producing types of `EnvVar`
(.=) :: Var a
     => String
     -> a
     -> EnvVar
(.=) x y = EnvVar (x, toVar y)

------------------------------------------------------------------------------
-- | `FromEnv` Typeclass w/ Generic default implementation
class FromEnv a where
  fromEnv :: Parser a
  fromEnvCustom :: (DefConfig a, Generic a, GFromEnv (Rep a)) => Option -> Parser a
  fromEnvCustom opts = to <$> gFromEnv (from (defConfig :: a)) opts
  default fromEnv :: (DefConfig a, Generic a, GFromEnv (Rep a)) => Parser a
  fromEnv = fromEnvCustom defOption

------------------------------------------------------------------------------
-- | `Generic` FromEnv
class GFromEnv f where
  gFromEnv :: f a -> Option -> Parser (f a)

------------------------------------------------------------------------------
-- | Default Config
class DefConfig a where defConfig :: a

------------------------------------------------------------------------------
-- | For customizing environment variable generation
data Option = Option {
    dropPrefixCount :: Int  -- ^ Applied first
  , customPrefix :: String  -- ^ Converted toUpper
  } deriving Show

------------------------------------------------------------------------------
-- | Default `Option` for field modification
defOption :: Option
defOption = Option 0 mempty

------------------------------------------------------------------------------
-- | Products
instance (GFromEnv a, GFromEnv b) => GFromEnv (a :*: b) where
  gFromEnv (a :*: b) opts = liftA2 (:*:) (gFromEnv a opts) (gFromEnv b opts)

------------------------------------------------------------------------------
-- | Don't absorb meta data
instance GFromEnv a => GFromEnv (C1 i a) where gFromEnv (M1 x) opts = M1 <$> gFromEnv x opts

------------------------------------------------------------------------------
-- | Don't absorb meta data
instance GFromEnv a => GFromEnv (D1 i a) where gFromEnv (M1 x) opts = M1 <$> gFromEnv x opts

------------------------------------------------------------------------------
-- | Construct a `Parser` from a `selName` and `DefConfig` record field
instance (Selector s, Typeable a, Var a) => GFromEnv (S1 s (K1 i a)) where
  gFromEnv m@(M1 (K1 def)) opts =
      M1 . K1 <$> envMaybe (toEnvName opts $ selName m) .!= def
    where
      toEnvName :: Option -> String -> String
      toEnvName Option{..} xs =
        let name = snake (drop dropPrefixCount xs)
        in if customPrefix == mempty
             then name
             else map toUpper customPrefix ++ "_" ++ name

      applyFirst :: (Char -> Char) -> String -> String
      applyFirst _ []     = []
      applyFirst f [x]    = [f x]
      applyFirst f (x:xs) = f x: xs

      snakeCase :: String -> String
      snakeCase = u . applyFirst toLower
        where u []                 = []
              u (x:xs) | isUpper x = '_' : toLower x : snakeCase xs
                       | otherwise = x : u xs

      snake :: String -> String
      snake = map toUpper . snakeCase

------------------------------------------------------------------------------
-- | ToEnv Typeclass
class ToEnv a where toEnv :: a -> EnvList a

------------------------------------------------------------------------------
-- | EnvList type w/ phanton
data EnvList a = EnvList [EnvVar] deriving (Show)

------------------------------------------------------------------------------
-- | smart constructor, Environment creation helper
makeEnv :: ToEnv a => [EnvVar] -> EnvList a
makeEnv = EnvList

------------------------------------------------------------------------------
-- | Class for converting to / from an environment variable
class Var a where
  toVar   :: a -> String
  fromVar :: String -> Maybe a

------------------------------------------------------------------------------
instance Var Text where toVar = T.unpack; fromVar = Just . T.pack
instance Var TL.Text where toVar = TL.unpack; fromVar = Just . TL.pack
instance Var BL8.ByteString where toVar = BL8.unpack; fromVar = Just . BL8.pack
instance Var B8.ByteString where toVar = B8.unpack; fromVar = Just . B8.pack
instance Var Int where toVar = show; fromVar = readMaybe
instance Var Int8 where toVar = show; fromVar = readMaybe
instance Var Int16 where toVar = show; fromVar = readMaybe
instance Var Int32 where toVar = show; fromVar = readMaybe
instance Var Int64 where toVar = show; fromVar = readMaybe
instance Var Integer where toVar = show; fromVar = readMaybe
instance Var UTCTime where toVar = show; fromVar = readMaybe
instance Var Day where toVar = show; fromVar = readMaybe
instance Var Word8 where toVar = show; fromVar = readMaybe
instance Var Bool where toVar = show; fromVar = readMaybe
instance Var Double where toVar = show; fromVar = readMaybe
instance Var Word16 where toVar = show; fromVar = readMaybe
instance Var Word32 where toVar = show; fromVar = readMaybe
instance Var Word64 where toVar = show; fromVar = readMaybe
instance Var String where toVar = id; fromVar = Just

------------------------------------------------------------------------------
-- | Environment retrieval with failure info
decodeEnv :: FromEnv a => IO (Either String a)
decodeEnv = evalParser fromEnv

------------------------------------------------------------------------------
-- | Environment retrieval (with no failure info)
decode :: FromEnv a => IO (Maybe a)
decode = fmap f decodeEnv
  where
    f (Left _)  = Nothing
    f (Right x) = Just x

------------------------------------------------------------------------------
-- | Set environment via a ToEnv constrained type
setEnvironment :: EnvList a -> IO (Either String ())
setEnvironment (EnvList xs) = do
  result <- try $ mapM_ (uncurry setEnv . getEnvVar) xs
  return $ case result of
   Left (ex :: IOException) -> Left (show ex)
   Right () -> Right ()

------------------------------------------------------------------------------
-- | Set environment directly using a value of class ToEnv
setEnvironment' :: ToEnv a => a -> IO (Either String ())
setEnvironment' = setEnvironment . toEnv

------------------------------------------------------------------------------
-- | Unset Environment from a `ToEnv` constrained type
unsetEnvironment :: ToEnv a => EnvList a -> IO (Either String ())
unsetEnvironment (EnvList xs) = do
  result <- try $ mapM_ (unsetEnv . fst . getEnvVar) xs
  return $ case result of
   Left (ex :: IOException) -> Left (show ex)
   Right () -> Right ()

------------------------------------------------------------------------------
-- | Display all environment variables, for convenience
showEnv :: IO ()
showEnv = mapM_ print =<< getEnvironment
