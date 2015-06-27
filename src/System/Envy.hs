{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RankNTypes          #-}
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
       ( -- * Types
         Env     (..)
        -- * Classes
       , FromEnv (..)
       , ToEnv   (..)
       , Var     (..)
        -- * Functions
       , loadEnv
       , decodeEnv
       , decode
       , showEnv
       , setEnvironment
       , unsetEnvironment
       , (.=)
       , (.:)
       , (.:?)
       , (.!=)
       ) where
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Exception
import           Data.Maybe
import           Data.Time
import           Data.Monoid
import           Control.Monad
import           Data.Typeable
import           System.Environment
import           Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Word
import           Data.Int
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Environment
newtype Env = Env { env :: M.Map String String }
  deriving (Show)

------------------------------------------------------------------------------
-- | Parser
newtype Parser a = Parser { runParser :: ReaderT Env (ExceptT String IO) a }
  deriving ( Functor, Monad, Applicative, MonadReader Env, MonadError String
           , MonadIO, Alternative, MonadPlus )

------------------------------------------------------------------------------
-- | Variable type, smart constructor for handling Env. Variables
data EnvVar = EnvVar { getEnvVar :: (String, String) }
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Execute Parser
evalParser :: FromEnv a => Parser a -> IO (Either String a)
evalParser stack = do
  env <- liftIO loadEnv
  runExceptT $ runReaderT (runParser stack) env

------------------------------------------------------------------------------
-- | Infix environment variable getter
getE
  :: forall a . (Typeable a, Var a)
  => String
  -> Env
  -> Parser a
getE k (Env m) = do
  case M.lookup k m of
    Nothing -> throwError $ "Variable not found for: " ++ k
    Just dv ->
      case fromVar dv :: Maybe a of
        Nothing -> throwError $ "Parse failure: field <name> is not of type: "
                     ++ show (typeOf dv)
        Just x -> return x

------------------------------------------------------------------------------
-- | Infix environment variable getter
getEMaybe
  :: forall a . (Typeable a, Var a)
  => String
  -> Env
  -> Parser (Maybe a)
getEMaybe k (Env m) = do
  return $ do
    dv <- M.lookup k m
    fromVar dv 

------------------------------------------------------------------------------
-- | Infix environment variable getter
(.:) :: forall a. (Typeable a, Var a)
  => String
  -> Env
  -> Parser a
(.:) = getE

------------------------------------------------------------------------------
-- | Infix environment variable setter 
-- this is a smart constructor for producing types of `EnvVar`
(.=) :: Var a
     => String
     -> a
     -> EnvVar 
(.=) x y = EnvVar (x, toVar y)

------------------------------------------------------------------------------
-- | Maybe parser
(.:?) :: forall a. (Typeable a, Var a)
  => String
  -> Env
  -> Parser (Maybe a)
(.:?) = getEMaybe

------------------------------------------------------------------------------
-- | For use with (.:?) for providing default arguments
(.!=) :: forall a. (Typeable a, Var a)
  => Parser (Maybe a)
  -> a
  -> Parser a          
(.!=) p x  = fmap (fromMaybe x) p

------------------------------------------------------------------------------
-- | FromEnv Typeclass
class FromEnv a where
  fromEnv :: Env -> Parser a

------------------------------------------------------------------------------
-- | Identity instance
instance FromEnv Env where fromEnv = return

------------------------------------------------------------------------------
-- | ToEnv Typeclass
class Show a => ToEnv a where
  toEnv :: a -> [EnvVar]

------------------------------------------------------------------------------
-- | Class for converting to / from an environment variable
class (Read a, Show a) => Var a where
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
-- | Environment loading
loadEnv :: IO Env
loadEnv = Env . M.fromList <$> getEnvironment

------------------------------------------------------------------------------
-- | Environment retrieval with failure info
decodeEnv :: FromEnv a => IO (Either String a)
decodeEnv = loadEnv >>= evalParser . fromEnv

------------------------------------------------------------------------------
-- | Environment retrieval (with no failure info)
decode :: FromEnv a => IO (Maybe a)
decode = fmap f $ loadEnv >>= evalParser . fromEnv
  where
    f (Left _) = Nothing
    f (Right x) = Just x

------------------------------------------------------------------------------
-- | Set environment via a ToEnv constrained type
setEnvironment :: ToEnv a => a -> IO (Either String ())
setEnvironment x = do
  result <- try $ mapM_ (uncurry setEnv) (map getEnvVar $ toEnv x)
  return $ case result of
   Left (ex :: IOException) -> Left (show ex)
   Right () -> Right ()

------------------------------------------------------------------------------
-- | Unset Environment from a ToEnv constrained type
unsetEnvironment :: ToEnv a => a -> IO (Either String ())
unsetEnvironment x = do
  result <- try $ mapM_ unsetEnv $ map fst (map getEnvVar $ toEnv x)
  return $ case result of
   Left (ex :: IOException) -> Left (show ex)
   Right () -> Right ()

------------------------------------------------------------------------------
-- | Env helper
showEnv :: Env -> IO ()
showEnv (Env xs) = mapM_ print (M.toList xs)
