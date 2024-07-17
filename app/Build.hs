{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Build where

import Config
import Control.Monad (void)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config
import Development.Shake.FilePath
import GHC.Generics (Generic)
import ShakeExtras
import System.Directory.Extra (copyFile)
import System.Environment (lookupEnv)
import Types

newtype Build = Build PackageName
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

type instance RuleResult Build = FilePath

buildRule :: Rules ()
buildRule = void $ addOracle $ \(Build packageName) -> do
  putInfo $ "Start building" <> T.unpack packageName
  PackageDesc {..} <- fromJust <$> lookupPackageDesc packageName
  ver@(versionName, versionCode) <- fromJust <$> lookupPackageBuildVersion packageName
  let descConfig =
        [ ("project_name", descProjectName),
          ("package_name", descPackageName),
          ("version_name", versionName),
          ("version_code", T.pack $ show versionCode),
          ("app_name_debug", descAppNameDebug),
          ("app_name_release", descAppNameRelease),
          ("plugin_description", descPluginDesc)
        ]
  libConfig <-
    mapM
      (\k -> (k,) . T.pack . fromJust <$> getConfig k)
      [ "aboutlibraries_version",
        "main_version",
        "desugarJDKLibs_version",
        "kotlin_version",
        "plugin_api_version"
      ]
  withTempDir $ \dir -> do
    liftIO $
      T.writeFile (dir </> "build.cfg") $
        T.unlines [T.pack (k <> " ") <> v | (k, v) <- descConfig <> libConfig]
    cmd_ "plugin-scaffold"
    putInfo "Running pre build"
    descPreBuild ver $ dir </> "out"
    liftIO (lookupEnv "IN_NIX_SHELL") >>= \case
      Just _ -> cmd_ "nix" "develop" "#noAS" "--command" "./gradlew" "assembleRelease"
      _ -> cmd_ "./gradlew" "assembleRelease"
    let releaseDir = dir </> "app" </> "build" </> "outputs" </> "apk" </> "release"
    files <- liftIO $ getDirectoryFilesIO releaseDir ["*.apk"]
    let apk = head files
    putInfo "Copying apk to build directory"
    liftIO $ copyFile (releaseDir </> apk) (buildDir </> "unsigned" </> apk)
    pure apk

-- | Returns the name of the apk file located in @buildDir@/unsigned
buildPackage :: PackageName -> Action FilePath
buildPackage = askOracle . Build
