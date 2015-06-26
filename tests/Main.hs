{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
------------------------------------------------------------------------------
module Main ( main ) where
------------------------------------------------------------------------------
import           Data.Time
import           System.Environment
import           Control.Applicative
import           Control.Monad
import           System.Envy
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Text as T
import           Data.Text    (Text)
import qualified Data.Text.Lazy as LT
import           Data.Word
import           Data.Int
import           Data.String
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Test.QuickCheck.Instances
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Posgtres port
newtype PGPORT = PGPORT Int
     deriving (Read, Show, Var, Eq, Typeable)

------------------------------------------------------------------------------
-- | Postgres URL
newtype PGURL = PGURL String
     deriving (Read, Show, Var, IsString, Eq, Typeable)

------------------------------------------------------------------------------
-- | Posgtres config
data PGConfig = PGConfig {
    pgPort :: PGPORT -- ^ Port 
  , pgURL  :: PGURL  -- ^ URL
  } deriving (Show, Read, Eq, Typeable)

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
main = hspec $ do 
  let pgConfig = PGConfig (PGPORT 5432) "localhost" 
  describe "Var ismorphisms hold" $ do
    it "Word8 Var isomorphism" $ property $ 
     \(x :: Word8) -> Just x == fromVar (toVar x)
    it "Word16 Var isomorphism" $ property $ 
     \(x :: Word16) -> Just x == fromVar (toVar x)
    it "Word32 Var isomorphism" $ property $ 
     \(x :: Word32) -> Just x == fromVar (toVar x)
    it "Int Var isomorphism" $ property $ 
     \(x :: Int) -> Just x == fromVar (toVar x)
    it "Int8 Var isomorphism" $ property $ 
     \(x :: Int8) -> Just x == fromVar (toVar x)
    it "Int16 Var isomorphism" $ property $ 
     \(x :: Int16) -> Just x == fromVar (toVar x)
    it "Int32 Var isomorphism" $ property $ 
     \(x :: Int32) -> Just x == fromVar (toVar x)
    it "Int64 Var isomorphism" $ property $ 
     \(x :: Int64) -> Just x == fromVar (toVar x)
    it "String Var isomorphism" $ property $ 
     \(x :: String) -> Just x == fromVar (toVar x)
    it "Double Var isomorphism" $ property $ 
     \(x :: Double) -> Just x == fromVar (toVar x)
    it "UTCTime Var isomorphism" $ property $ 
     \(x :: UTCTime) -> Just x == fromVar (toVar x)
    it "ByteString Var isomorphism" $ property $ 
     \(x :: B8.ByteString) -> Just x == fromVar (toVar x)
    it "ByteString Var isomorphism" $ property $ 
     \(x :: BL8.ByteString) -> Just x == fromVar (toVar x)
    it "Lazy Text Var isomorphism" $ property $ 
     \(x :: LT.Text) -> Just x == fromVar (toVar x)
    it "Text Var isomorphism" $ property $ 
     \(x :: T.Text) -> Just x == fromVar (toVar x)
  describe "Can set to and from environment" $ do
    it "Should set environment" $ do
      setEnvironment pgConfig
      Right pg' <- parseEnv :: IO (Either String PGConfig)
      pg' `shouldBe` pgConfig

