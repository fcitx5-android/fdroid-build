{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Build where

import Control.Monad (void)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics (Generic)
import ShakeExtras
import Types
import Config

newtype Build = Build PackageName
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

type instance RuleResult Build = FilePath

buildRule :: Rules ()
buildRule = void $ addOracle $ \(Build packageName) -> do
  PackageDesc {..} <- fromJust <$> lookupPackageDesc packageName
  Config{..} <- getConfig
  (versionName, versionCode) <- fromJust <$> getPackageVersion packageName
  withTempDir $ \dir ->
    liftIO $
      T.writeFile (dir </> "build.cfg") $
        T.unlines
          [ k <> " " <> v
            | (k, v) <-
                [ ("project_name", descProjectName),
                  ("package_name", descPackageName),
                  ("version_name", versionName),
                  ("version_code", T.pack $ show versionCode),
                  ("app_name_debug", descAppNameDebug),
                  ("app_name_release", descAppNameRelease),
                  ("plugin_description", descPluginDesc),
                  ("aboutlibraries_version", aboutlibrariesVersion),
                  ("main_version", mainVersion),
                  ("desugarJDKLibs_version", desugarJDKLibsVersion),
                  ("kotlin_version", kotlinVersion),
                  ("build_commit_hash", "unknown"),
                  ("plugin_api_version", "0.1")
                ]
          ]
  pure $ Just ()
