{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
------------------------------------------------------------------------------
-- |
-- Module      : System.Envy
-- Copyright   : (c) David Johnson 2015
-- Maintainer  : djohnson.m@gmail.com
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
         FromEnv    (..)
       , ToEnv      (..)
       , Var        (..)
       , EnvList    (..)
       , EnvVar     (..)
       , Parser     (..)
       , ParseError (..)
        -- * Functions
       , decodeEnv
       , decodeWithDefaults
       , decode
       , showEnv
       , setEnvironment
       , setEnvironment'
       , unsetEnvironment
       , unsetEnvironment'
       , makeEnv
       , env
       , envMaybe
       , (.=)
       , (.!=)
         -- * Utility Types
       , ReadShowVar (..)
       , (.?=)
         -- * Generics
       , DefConfig (..)
       , Option (..)
       , defOption
       , runEnv
       , gFromEnvCustom
       ) where
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Exception
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Monoid
import qualified Data.Semigroup as S
import           Data.Char
import           Data.Time
import           GHC.Generics
import           Data.Typeable
import           System.Environment.Blank
import           Text.Read (readEither)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import           Data.Void
import           Data.Word
import           Data.Int
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
------------------------------------------------------------------------------
data ParseError =
    ParseFailed
      !String {- ^ Variable name -}
      !TypeRep {- ^ Type tried to parse as -}
      !String {- ^ Error by 'readEither' -}
  | VariableNotFound !String {- ^ Variable name -}
  | Fail !String
  deriving (Eq, Show)

instance S.Semigroup ParseError where
  a <> _ = a

instance Monoid ParseError where
  mempty = ParseFailed "<NONE>" (typeRep (Proxy :: Proxy Void)) "mempty"

-- | Parser Monad for environment variable retrieval
newtype Parser a = Parser { runParser :: ExceptT ParseError IO a }
  deriving ( Functor, Monad, Applicative, MonadError ParseError
           , MonadIO, Alternative, MonadPlus )

instance MonadFail Parser where
  fail = Parser . throwError . Fail

------------------------------------------------------------------------------
-- | Variable type, smart constructor for handling environment variables.
data EnvVar = EnvVar {
  variableName :: String,
  -- ^ The environment variable to set.
  variableValue :: String
  -- ^ The value to assign this variable to.
  }
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | For use with Generics, no `FromEnv` typeclass necessary
--
-- > getPgConfig :: IO (Either String ConnectInfo)
-- > getPgConfig = runEnv $ gFromEnvCustom defOption
runEnv :: Parser a -> IO (Either ParseError a)
runEnv = runExceptT . runParser

------------------------------------------------------------------------------
-- | Environment variable getter. Fails if the variable is not set or
-- fails to parse.
env :: Var a
    => String   -- ^ Key to look up.
    -> Parser a -- ^ Return a value of this type or throw an error.
env key = do
  result <- liftIO (getEnv key)
  case result of
    Nothing -> throwError $ VariableNotFound key
    Just dv ->
      case fromVar dv of
        Left emsg ->
          throwError $ ParseFailed key (typeOf dv) emsg
        Right x -> return x

------------------------------------------------------------------------------
-- | Environment variable getter returning `Maybe`
-- TODO deprecate?
envMaybe :: Var a
         => String           -- ^ Key to look up.
         -> Parser (Maybe a) -- ^ Return `Nothing` if variable isn't set.
envMaybe key = (Just <$> env key) `catchError` h
 where
  h (VariableNotFound _) = return Nothing
  h other = throwError other

------------------------------------------------------------------------------
-- | For use with `envMaybe` for providing default arguments.
-- TODO deprecate?
(.!=) :: Parser (Maybe a) -- ^ Parser that might fail.
      -> a                -- ^ Value to return if the parser fails.
      -> Parser a         -- ^ Parser that returns the default on failure.
(.!=) parser def  = fromMaybe def <$> parser

------------------------------------------------------------------------------
-- | For use with `env` for providing default arguments.
(.?=) :: Parser a
      -> a        -- ^ Value to return if the environment variable is not found.
      -> Parser a -- ^ Parser that returns the default if the environment variable is not found.
(.?=) parser def = parser `catchError` h
 where
  h (VariableNotFound _) = return def
  h other = throwError other

------------------------------------------------------------------------------
-- | Infix environment variable setter
-- Smart constructor for producing types of `EnvVar`
(.=) :: Var a
     => String -- ^ The variable name to set.
     -> a      -- ^ Object to set in the environment.
     -> EnvVar -- ^ Mapping of Variable to Value.
(.=) variableName value = EnvVar variableName (toVar value)

------------------------------------------------------------------------------
-- | `FromEnv` Typeclass w/ Generic default implementation
class FromEnv a where
  fromEnv :: Maybe a -> Parser a
  default fromEnv :: (Generic a, GFromEnv (Rep a)) => Maybe a -> Parser a
  fromEnv oa = gFromEnvCustom defOption oa

------------------------------------------------------------------------------
-- | Meant for specifying a custom `Option` for environment retrieval
--
-- > instance FromEnv PGConfig where
-- >   fromEnv = gFromEnvCustom Option { dropPrefixCount = 8, customPrefix = "PG" }
gFromEnvCustom :: forall a. (Generic a, GFromEnv (Rep a))
               => Option
               -> Maybe a
               -> Parser a
gFromEnvCustom opts oa = to <$> gFromEnv opts (from <$> oa)

------------------------------------------------------------------------------
-- | `Generic` FromEnv
class GFromEnv f where
  gFromEnv :: Option -> Maybe (f a) ->  Parser (f a)

------------------------------------------------------------------------------
-- | Type class for objects which have a default configuration.
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
  gFromEnv opts ox = let (oa, ob) = case ox of
                                             (Just (a :*: b)) -> (Just a, Just b)
                                             _ -> (Nothing, Nothing) in
                            liftA2 (:*:) (gFromEnv opts oa) (gFromEnv opts ob)

------------------------------------------------------------------------------
-- | Don't absorb meta data
instance GFromEnv a => GFromEnv (C1 i a) where
  gFromEnv  opts (Just (M1 x))= M1 <$> gFromEnv opts (Just x)
  gFromEnv  opts            _ = M1 <$> gFromEnv opts Nothing

------------------------------------------------------------------------------
-- | Don't absorb meta data
instance GFromEnv a => GFromEnv (D1 i a) where
  gFromEnv opts (Just (M1 x)) = M1 <$> gFromEnv opts (Just x)
  gFromEnv opts             _ = M1 <$> gFromEnv opts Nothing

------------------------------------------------------------------------------
-- | Construct a `Parser` from a `selName` and `DefConfig` record field
instance (Selector s, Var a) => GFromEnv (S1 s (K1 i a)) where
  gFromEnv opts ox =
    let p = case ox of
               Just (M1 (K1 def)) -> envMaybe envName .!= def
               _                  -> env envName in
    M1 . K1 <$> p
     where
      envName = toEnvName opts $ selName (SelectorProxy :: SelectorProxy s Proxy ())

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

data SelectorProxy (s :: Meta) (f :: * -> *) a = SelectorProxy

------------------------------------------------------------------------------
-- | Type class for objects which can be converted to a set of
-- environment variable settings.
class ToEnv a where
  -- | Convert an object into a list of environment variable settings.
  toEnv :: a -> EnvList a

------------------------------------------------------------------------------
-- | List of environment variables. Captures a "phantom type" which
-- allows the type checker to detect the proper implementation of toEnv
-- to use.
data EnvList a = EnvList [EnvVar] deriving (Show)

------------------------------------------------------------------------------
-- | Smart constructor, environment creation helper.
makeEnv :: [EnvVar] -> EnvList a
makeEnv = EnvList

------------------------------------------------------------------------------
-- | Class for converting to / from an environment variable
class Typeable a => Var a where
  -- | Convert a value into an environment variable.
  toVar   :: a -> String
  -- | Parse an environment variable.
  --   The error message is (usually) produced by 'readEither'
  fromVar :: String -> Either String a

------------------------------------------------------------------------------
instance Var Text where toVar = T.unpack; fromVar = Right . T.pack
instance Var TL.Text where toVar = TL.unpack; fromVar = Right . TL.pack
instance Var BL8.ByteString where toVar = BL8.unpack; fromVar = Right . BL8.pack
instance Var B8.ByteString where toVar = B8.unpack; fromVar = Right . B8.pack
instance Var Int where toVar = show; fromVar = readEither
instance Var Int8 where toVar = show; fromVar = readEither
instance Var Int16 where toVar = show; fromVar = readEither
instance Var Int32 where toVar = show; fromVar = readEither
instance Var Int64 where toVar = show; fromVar = readEither
instance Var Integer where toVar = show; fromVar = readEither
instance Var UTCTime where toVar = show; fromVar = readEither
instance Var Day where toVar = show; fromVar = readEither
instance Var Word8 where toVar = show; fromVar = readEither
instance Var Bool where toVar = show; fromVar = readEither
instance Var Double where toVar = show; fromVar = readEither
instance Var Word16 where toVar = show; fromVar = readEither
instance Var Word32 where toVar = show; fromVar = readEither
instance Var Word64 where toVar = show; fromVar = readEither
instance Var String where toVar = id; fromVar = Right
instance Var () where toVar = const "()"; fromVar = const $ Right ()
instance Var a => Var (Maybe a) where
  toVar = maybe "" toVar
  fromVar "" = Left "empty value"
  fromVar  s = Just <$> fromVar s

------------------------------------------------------------------------------
deriving instance (Var a, Typeable a) => Var (Last a)
deriving instance (Var a, Typeable a) => Var (First a)
deriving instance (Var a, Typeable a) => Var (Identity a)

------------------------------------------------------------------------------
-- | A utility type to use any instance of 'Read' and 'Show' as an instance of
--   'Var'.
newtype ReadShowVar a = ReadShowVar { unReadShowVar :: a }

instance (Typeable a, Show a, Read a) => Var (ReadShowVar a) where
  toVar = show . unReadShowVar
  fromVar = fmap ReadShowVar . readEither
------------------------------------------------------------------------------
-- | Environment retrieval with failure info
decodeEnv :: FromEnv a => IO (Either ParseError a)
decodeEnv = runEnv (fromEnv Nothing)

------------------------------------------------------------------------------
-- | Environment retrieval (with no failure info)
decode :: FromEnv a => IO (Maybe a)
decode = fmap eitherToMaybe decodeEnv
  where
    eitherToMaybe (Left _)  = Nothing
    eitherToMaybe (Right x) = Just x

------------------------------------------------------------------------------
-- | Environment retrieval with default values provided
decodeWithDefaults :: FromEnv a => a -> IO a
decodeWithDefaults def = (\(Right x) -> x) <$> runEnv (fromEnv (Just def))

------------------------------------------------------------------------------
-- | Catch an IO exception and return it in an Either.
wrapIOException :: IO a -> IO (Either String a)
wrapIOException action = try action >>= \case
  Left (ex :: IOException) -> return $ Left $ show ex
  Right x -> return $ Right x

------------------------------------------------------------------------------
-- | Set environment via a ToEnv constrained type
setEnvironment :: EnvList a -> IO (Either String ())
setEnvironment (EnvList envVars) = wrapIOException $ mapM_ set envVars
  where set var = setEnv (variableName var) (variableValue var) True

------------------------------------------------------------------------------
-- | Set environment directly using a value of class ToEnv
setEnvironment' :: ToEnv a => a -> IO (Either String ())
setEnvironment' = setEnvironment . toEnv

------------------------------------------------------------------------------
-- | Unset Environment from a `ToEnv` constrained type
unsetEnvironment :: EnvList a -> IO (Either String ())
unsetEnvironment (EnvList envVars) = wrapIOException $ mapM_ unset envVars
  where unset var = unsetEnv (variableName var)

------------------------------------------------------------------------------
-- | Unset Environment using a value of class ToEnv
unsetEnvironment' :: ToEnv a => a -> IO (Either String ())
unsetEnvironment' = unsetEnvironment . toEnv

------------------------------------------------------------------------------
-- | Display all environment variables, for convenience
showEnv :: IO ()
showEnv = mapM_ print =<< getEnvironment
