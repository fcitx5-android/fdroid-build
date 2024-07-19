{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Build where

import Config
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config
import Development.Shake.FilePath
import GHC.Generics (Generic)
import ShakeExtras
import System.Directory.Extra (copyFile, createDirectoryIfMissing, renameFile)
import Types

newtype Build = Build PackageName
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

type instance RuleResult Build = FilePath

buildRule :: Rules ()
buildRule = void $ addOracle $ \(Build packageName) -> do
  putInfo $ "Start building" <> T.unpack packageName
  PackageDesc {..} <- getPackageDesc packageName
  ver@(_, versionName, versionCode) <- getPackageBuildVersion packageName
  let descConfig =
        [ ("project_name", descProjectName),
          ("package_name", descPackageName),
          ("build_version_name", versionName),
          ("version_code", T.pack $ show versionCode),
          ("app_name_debug", descAppNameDebug),
          ("app_name_release", descAppNameRelease),
          ("plugin_description", descPluginDesc)
        ]
  libConfig <-
    let f k (Just s) = (k, T.pack s)
        f k Nothing = error $ "Config " <> k <> " not found"
     in mapM
          (\k -> f k <$> getConfig k)
          [ "aboutlibraries_version",
            "main_version",
            "desugarJDKLibs_version",
            "kotlin_version",
            "plugin_api_version",
            "android_version",
            "build_commit_hash"
          ]
  withTempDir $ \dir -> do
    liftIO $
      T.writeFile (dir </> "build.cfg") $
        T.unlines [T.pack (k <> " = ") <> v | (k, v) <- descConfig <> libConfig]
    cmd_ (Cwd dir) "plugin-scaffold"
    let root = dir </> "out"
    putInfo $ "Running pre build in " <> root
    descPreBuild ver root
    cmd_ (Cwd root) "chmod" "+x" "gradlew"
    cmd_ (Cwd root) "./gradlew" "assembleRelease"
    let releaseDir = root </> "app" </> "build" </> "outputs" </> "apk" </> "release"
    let unsignedApk = T.unpack packageName <> "-" <> T.unpack versionName <> "-" <> "release" <> "-" <> "unsigned" <.> "apk"
        apk = T.unpack packageName <> "-" <> T.unpack versionName <> "-" <> "release" <.> "apk"
    putInfo $ "Renaming " <> unsignedApk <> " to " <> apk
    liftIO $ renameFile (releaseDir </> unsignedApk) (releaseDir </> apk)
    putInfo $ "Copying " <> apk <> " to build directory"
    liftIO $ createDirectoryIfMissing True (buildDir </> "unsigned")
    liftIO $ copyFile (releaseDir </> apk) (buildDir </> "unsigned" </> apk)
    pure apk

-- | Returns the name of the apk file located in @buildDir@/unsigned
buildPackage :: PackageName -> Action FilePath
buildPackage = askOracle . Build
