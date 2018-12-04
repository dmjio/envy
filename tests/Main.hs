{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
------------------------------------------------------------------------------
module Main ( main ) where
------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Int
import           Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Time
import           Data.Word
import           GHC.Generics
import           System.Envy
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Monadic



------------------------------------------------------------------------------
data UserInfo = UserInfo {
      userName :: String
    , userAge :: Int
  } deriving (Show, Eq, Generic)


-- | Generic instance
instance FromEnv UserInfo

instance Arbitrary UserInfo where
  arbitrary = UserInfo <$> arbitrary <*> arbitrary

------------------------------------------------------------------------------
data ConnectInfo = ConnectInfo {
      pgHost :: String
    , pgPort :: Word16
    , pgUser :: String
    , pgPass :: String
    , pgDB   :: String
  } deriving (Show, Eq, Generic)


instance DefConfig ConnectInfo where
  defConfig = ConnectInfo "localhost" 5432 "user" "pass" "db"

-- | Generic instance with default values
instance FromEnv ConnectInfo

instance Arbitrary ConnectInfo where
    arbitrary = ConnectInfo <$> nonulls
                            <*> arbitrary
                            <*> nonulls
                            <*> nonulls
                            <*> nonulls
      where nonempty = getNonEmpty <$> arbitrary
            nonulls = nonempty `suchThat` (not . ('\NUL' `elem`))

------------------------------------------------------------------------------
-- | Posgtres config
data PGConfig = PGConfig {
    pgConnectInfo :: ConnectInfo -- ^ Connnection Info
  } deriving (Eq)

instance Arbitrary PGConfig where
    arbitrary = PGConfig <$> arbitrary

------------------------------------------------------------------------------
-- | Custom show instance
instance Show PGConfig where
  show PGConfig {..} = "<PGConfig>"

------------------------------------------------------------------------------
-- | FromEnv Instances, supports popular aeson combinators *and* IO
-- for dealing with connection pools
instance FromEnv PGConfig where
  fromEnv _ = PGConfig <$> (ConnectInfo <$> envMaybe "PG_HOST" .!= "localhost"
                                        <*> env "PG_PORT"
                                        <*> env "PG_USER"
                                        <*> env "PG_PASS"
                                        <*> env "PG_DB")

------------------------------------------------------------------------------
-- | To Environment Instances
instance ToEnv PGConfig where
  toEnv PGConfig{..} = let ConnectInfo{..} = pgConnectInfo
                       in
                       makeEnv [ "PG_HOST" .= pgHost
                               , "PG_PORT" .= pgPort
                               , "PG_USER" .= pgUser
                               , "PG_PASS" .= pgPass
                               , "PG_DB"   .= pgDB
                               ]

------------------------------------------------------------------------------
-- | Start tests
main :: IO ()
main = hspec $ do
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
    it "() Var isomorphism" $ property $
     \(x :: ()) -> Just x == fromVar (toVar x)
  describe "Can set to and from environment" $ do
    it "Isomorphism through setEnvironment['] and decodeEnv" $ property $
      \(pgConf::PGConfig) -> monadicIO $ do
        res <- run $ do
                 _ <- setEnvironment' pgConf
                 decodeEnv
        assert $ res == Right pgConf
    it "Storing and retrieving var" $ property $
      \(x::String) -> monadicIO $ do
        res <- run $ do
                 _ <- setEnvironment $ makeEnv [ "HSPECENVY_" .= toVar x]
                 runEnv $ env  "HSPECENVY_"
        assert $ if isInfixOf "\NUL" x then True else res == Right x
  describe "Can use generic FromEnv" $
    it "Isomorphism through setEnvironment and decodeEnv" $ property $
      \(ci::ConnectInfo) -> monadicIO $ do
        res <- run $ do
                 let ConnectInfo{..} = ci
                 _ <- setEnvironment $
                          makeEnv [ "PG_HOST" .= pgHost
                                  , "PG_PORT" .= pgPort
                                  , "PG_USER" .= pgUser
                                  , "PG_PASS" .= pgPass
                                  , "PG_DB"   .= pgDB
                                  ]
                 decodeWithDefaults (defConfig :: ConnectInfo)
        assert $ res == ci
  describe "Can use generic FromEnvNoDefault" $
    it "Isomorphism through setEnvironment and decodeEnv" $ property $
      \(u::UserInfo) -> monadicIO $ do
        res <- run $ do
                 let UserInfo{..} = u
                 _ <- setEnvironment $
                          makeEnv [ "USER_NAME" .= (userName ++ "")
                                  , "USER_AGE" .= userAge
                                  ]
                 decodeEnv
        assert $  if isInfixOf "\NUL" (userName u) then True else res == Right u
