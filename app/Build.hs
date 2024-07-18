{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Build where

import Config
import Control.Monad (void)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config
import Development.Shake.FilePath
import GHC.Generics (Generic)
import Paths_fdroid_build (getDataFileName)
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
          ("build_version_name", versionName),
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
    -- Run command in nix shell if we are in nix shell
    isInNixShell <- liftIO $ isJust <$> lookupEnv "IN_NIX_SHELL"
    if isInNixShell
      then do
        -- Git repo is required for nix flake
        cmd_ (Cwd root) "git init"
        cmd_ (Cwd root) "git add ."
        cmd_ (Cwd root) "nix" "develop" ".#noAS" "--command" "./gradlew" "assembleRelease"
      else cmd_ (Cwd root) "./gradlew" "assembleRelease"
    let releaseDir = root </> "app" </> "build" </> "outputs" </> "apk" </> "release"
    unsignedApk <- liftIO $ head <$> getDirectoryFilesIO releaseDir ["*.apk"]
    putInfo $ "Singing " <> unsignedApk
    signer <- liftIO $ getDataFileName "sign-apk.sh"
    (Stdout (takeFileName -> signedApk)) <-
      if isInNixShell
        then cmd (Cwd root) "nix" "develop" ".#noAS" "--command" signer (releaseDir </> unsignedApk)
        else cmd (Cwd root) signer (releaseDir </> unsignedApk)
    putInfo $ "Copying " <> signedApk <> " to build directory"
    liftIO $ copyFile (releaseDir </> signedApk) (buildDir </> signedApk)
    pure signedApk

-- | Returns the name of the apk file located in @buildDir@/unsigned
buildPackage :: PackageName -> Action FilePath
buildPackage = askOracle . Build
