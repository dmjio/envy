{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RankNTypes        #-}
------------------------------------------------------------------------------
module System.Envy
       ( -- * Types
         Env     (..)
       , Parser  (..)
        -- * Classes
       , FromEnv (..)
       , ToEnv   (..)
        -- * Functions
       , loadEnv
       , parseEnv
       , setEnvironment
       , unsetEnvironment
       , (.=)
       , (.:)
       ) where
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Typeable
import           System.Environment
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class 
import           Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Map as M
import           System.IO.Unsafe
import           Data.Text (Text)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Environemnt Type
newtype Env = Env (M.Map String String)
  deriving (Show)

------------------------------------------------------------------------------
-- | Failure continuation.
type Failure f r   = String -> f r

------------------------------------------------------------------------------
-- | Success continuation.
type Success a f r = a -> f r

------------------------------------------------------------------------------
-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    } 

------------------------------------------------------------------------------
-- | Parser `Monad` Instance
instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

------------------------------------------------------------------------------
-- | Parser `Functor` Instance
instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

------------------------------------------------------------------------------
-- | Parser `Applicative` Instance
instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

------------------------------------------------------------------------------
-- | Parser `Alternative` Instance
instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

------------------------------------------------------------------------------
-- | Parser `MonadPlus` Instance
instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

------------------------------------------------------------------------------
-- | Applicative Parser helper
apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

------------------------------------------------------------------------------
-- | Infix environemnt variable getter
getE
  :: forall a. (Typeable a, Read a)
  => String
  -> Env
  -> Parser a
getE k (Env m) = do
  case M.lookup k m of
    Nothing -> fail $ "Variable not found for: " ++ k
    Just dv -> case readMaybe dv :: Maybe a of
                 Nothing -> fail $ "Parse failure: field <name> is not of type: "
                              ++ show (typeOf dv)
                 Just x -> return x

------------------------------------------------------------------------------
-- | Infix environemnt variable getter
(.:) :: forall a. (Typeable a, Read a)
  => String
  -> Env
  -> Parser a
(.:) = getE

------------------------------------------------------------------------------
-- | Infix environemnt variable setter
(.=) :: Show a
     => String
     -> a
     -> (String, String)
(.=) x y = (x, show y)

------------------------------------------------------------------------------
-- | FromEnv Typeclass
class FromEnv a where fromEnv :: Env -> Parser a

------------------------------------------------------------------------------
-- | Identity instance
instance FromEnv Env where fromEnv = return

------------------------------------------------------------------------------
-- | ToEnv Typeclass
class Show a => ToEnv a where
  toEnv :: a -> [(String, String)]

------------------------------------------------------------------------------
-- | Environment retrieval
loadEnv :: IO Env
loadEnv = Env . M.fromList <$> getEnvironment

------------------------------------------------------------------------------
-- | Environment retrieval
parseEnv :: FromEnv a => IO (Either String a)
parseEnv = do
   ev <- loadEnv
   return $ runParser (fromEnv ev) Left Right

------------------------------------------------------------------------------
-- | Set Environment nfrom a ToEnv instance
setEnvironment :: ToEnv a => a -> IO ()
setEnvironment = mapM_ (uncurry setEnv) . toEnv 

------------------------------------------------------------------------------
-- | Set Environment from a ToEnv instance
unsetEnvironment :: ToEnv a => a -> IO ()
unsetEnvironment = mapM_ unsetEnv . map fst . toEnv 

------------------------------------------------------------------------------
-- | Print Env
printEnv :: Env -> IO ()
printEnv (Env xs) = mapM_ print (M.toList xs)
