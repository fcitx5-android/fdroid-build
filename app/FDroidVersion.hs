{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module FDroidVersion where

import Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Binary.Instances.UnorderedContainers ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Network.HTTP.Client (httpLbs, parseRequest, responseBody)
import ShakeExtras
import Types

newtype GetFDroidVersion = GetFDroidVersion PackageName
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

data GetFDroidIndex = GetFDroidIndex
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

newtype FDroidIndex = FDroidIndex (HashMap PackageName PackageVersion)
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

type instance RuleResult GetFDroidIndex = FDroidIndex

type instance RuleResult GetFDroidVersion = Maybe PackageVersion

instance A.FromJSON FDroidIndex where
  parseJSON = A.withObject "FDroidIndex" $ \o -> do
    packages <- o A..: "packages"
    FDroidIndex <$> A.withObject "packages" (fmap HMap.fromList . mapM (parsePackage . snd) . A.toList) packages
    where
      parsePackage = A.withObject "package" $ \o -> do
        versionName <- o A..: "versionName"
        versionCode <- o A..: "versionCode"
        pure (versionName, versionCode)

fdroidVersionRule :: Rules ()
fdroidVersionRule = void $ do
  f <- addOracle $ \GetFDroidIndex -> do
    manager <- getHttpManager
    request <- liftIO $ parseRequest "https://f5a.torus.icu/fdroid/repo/index-v1.json"
    liftIO (httpLbs request manager)
      >>= ( \case
              Left err -> error err
              Right index -> pure index
          )
        . A.eitherDecode @FDroidIndex
        . responseBody
  addOracle $ \(GetFDroidVersion packageName) -> do
    FDroidIndex index <- f GetFDroidIndex
    pure $ HMap.lookup packageName index

getLatestFDroidVersion :: PackageName -> Action (Maybe (VersionName, VersionCode))
getLatestFDroidVersion = askOracle . GetFDroidVersion
