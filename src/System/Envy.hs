{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
------------------------------------------------------------------------------
-- |
-- Module      : System.Envy
-- Copyright   : (c) David Johnson 2015
-- Maintainer  : djohnson.m@ngmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
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
       ) where
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Except
import           Control.Exception
import           Data.Maybe
import           Data.Time
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
-- | Parser
newtype Parser a = Parser { runParser :: ExceptT String IO a }
  deriving ( Functor, Monad, Applicative, MonadError String
           , MonadIO, Alternative, MonadPlus )
------------------------------------------------------------------------------
-- | Variable type, smart constructor for handling Env. Variables
data EnvVar = EnvVar { getEnvVar :: (String, String) }
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Execute Parser
evalParser :: FromEnv a => Parser a -> IO (Either String a)
evalParser = runExceptT . runParser

------------------------------------------------------------------------------
-- | Infix environment variable getter
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
-- | Infix environment variable getter
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
-- | Maybe parser
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
-- this is a smart constructor for producing types of `EnvVar`
(.=) :: Var a
     => String
     -> a
     -> EnvVar 
(.=) x y = EnvVar (x, toVar y)

------------------------------------------------------------------------------
-- | FromEnv Typeclass
class FromEnv a where
  fromEnv :: Parser a

------------------------------------------------------------------------------
-- | ToEnv Typeclass
class Show a => ToEnv a where
  toEnv :: a -> EnvList a

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

instance Var Text where
  toVar = T.unpack
  fromVar = Just . T.pack

instance Var TL.Text where
  toVar = TL.unpack
  fromVar = Just . TL.pack 

instance Var BL8.ByteString where
  toVar = BL8.unpack
  fromVar = Just . BL8.pack

instance Var B8.ByteString where
  toVar = B8.unpack
  fromVar = Just . B8.pack

instance Var Int where
  toVar = show
  fromVar = readMaybe 

instance Var Int8 where
  toVar = show
  fromVar = readMaybe 

instance Var Int16 where
  toVar = show
  fromVar = readMaybe 

instance Var Int32 where
  toVar = show
  fromVar = readMaybe 

instance Var Int64 where
  toVar = show
  fromVar = readMaybe 

instance Var Integer where
  toVar = show
  fromVar = readMaybe 

instance Var UTCTime where
  toVar = show
  fromVar = readMaybe 

instance Var Day where
  toVar = show
  fromVar = readMaybe 

instance Var Word8 where
  toVar = show
  fromVar = readMaybe 

instance Var Bool where
  toVar = show
  fromVar = readMaybe 

instance Var Double where
  toVar = show
  fromVar = readMaybe 

instance Var Word16 where
  toVar = show
  fromVar = readMaybe 

instance Var Word32 where
  toVar = show
  fromVar = readMaybe 

instance Var Word64 where
  toVar = show
  fromVar = readMaybe 

instance Var String where
  toVar = id
  fromVar = Just 

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
-- | Unset Environment from a ToEnv constrained type
unsetEnvironment :: EnvList a -> IO (Either String ())
unsetEnvironment (EnvList xs) = do
  result <- try $ mapM_ (unsetEnv . fst . getEnvVar) xs
  return $ case result of
   Left (ex :: IOException) -> Left (show ex)
   Right () -> Right ()

------------------------------------------------------------------------------
-- | Env helper
showEnv :: IO ()
showEnv = mapM_ print =<< getEnvironment
