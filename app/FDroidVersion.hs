{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module FDroidVersion where

import Config (buildDir)
import Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Data.Binary.Instances.UnorderedContainers ()
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Vector as V
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Download (downloadFile)
import GHC.Generics (Generic)
import Types

newtype GetFDroidVersion = GetFDroidVersion PackageName
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

data GetFDroidIndex = GetFDroidIndex
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

newtype FDroidIndex = FDroidIndex (HashMap PackageName FDroidVersion)
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

type FDroidVersion = (VersionName, VersionCode)

type instance RuleResult GetFDroidIndex = FDroidIndex

type instance RuleResult GetFDroidVersion = Maybe FDroidVersion

instance A.FromJSON FDroidIndex where
  parseJSON = A.withObject "FDroidIndex" $ \o -> do
    packages <- o A..: "packages"
    FDroidIndex <$> A.withObject "packages" (fmap HMap.fromList . mapM (\(k, v) -> (A.toText k,) <$> parsePackageArray v) . A.toList) packages
    where
      parsePackageArray = A.withArray "packageArray" $ \a -> parsePackage (V.head a)
      parsePackage = A.withObject "package" $ \o -> do
        versionName <- o A..: "versionName"
        versionCode <- o A..: "versionCode"
        pure (versionName, versionCode)

fdroidVersionRule :: Rules ()
fdroidVersionRule = void $ do
  f <- addOracle $ \GetFDroidIndex -> do
    downloadFile "https://f5a.torus.icu/fdroid/repo/index-v1.json" (buildDir </> "fdroid.json")
    putInfo "Downloading FDroid index"
    liftIO (A.eitherDecodeFileStrict @FDroidIndex (buildDir </> "fdroid.json"))
      >>= \case
        Left err -> error err
        Right index -> pure index
  addOracle $ \(GetFDroidVersion packageName) -> do
    FDroidIndex index <- f GetFDroidIndex
    pure $ HMap.lookup packageName index

getLatestFDroidVersion :: PackageName -> Action (Maybe FDroidVersion)
getLatestFDroidVersion = askOracle . GetFDroidVersion
