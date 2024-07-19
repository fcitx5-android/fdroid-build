{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module ShakeExtras where

import Control.Concurrent.Extra
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Development.Shake
import Types

data ShakeExtras = ShakeExtras
  { packageDesc :: HashMap PackageName PackageDesc,
    -- version we are going to build, set in Core rule
    packageBuildVersion :: Var (HashMap PackageName PackageVersion),
    packageBuilt :: Var (HashMap PackageName FilePath)
  }

initShakeExtras :: [PackageDesc] -> IO ShakeExtras
initShakeExtras pkgs = do
  packageBuildVersion <- newVar HMap.empty
  packageBuilt <- newVar HMap.empty
  let packageDesc = HMap.fromList $ map (\p -> (descPackageName p, p)) pkgs
  pure $ ShakeExtras {..}

getShakeExtras :: Action ShakeExtras
getShakeExtras =
  getShakeExtra @ShakeExtras >>= \case
    Just v -> pure v
    Nothing -> fail "ShakeExtras not found"

getPackageDesc :: PackageName -> Action PackageDesc
getPackageDesc pkgName = do
  ShakeExtras {..} <- getShakeExtras
  liftIO (pure (HMap.lookup pkgName packageDesc)) >>= \case
    Just v -> pure v
    Nothing -> fail $ "Package " <> show pkgName <> " not found in packageDesc"

setPackageBuildVersion :: PackageName -> PackageVersion -> Action ()
setPackageBuildVersion pkgName pkgVersion = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ modifyVar_ packageBuildVersion $ pure . HMap.insert pkgName pkgVersion

getPackageBuildVersion :: PackageName -> Action PackageVersion
getPackageBuildVersion pkgName = do
  ShakeExtras {..} <- getShakeExtras
  liftIO (HMap.lookup pkgName <$> readVar packageBuildVersion) >>= \case
    Just v -> pure v
    Nothing -> fail $ "Package " <> show pkgName <> " not found in build version"

markPackageBuilt :: PackageName -> FilePath -> Action ()
markPackageBuilt pkgName path = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ modifyVar_ packageBuilt $ pure . HMap.insert pkgName path

getPackagesBuilt :: Action (HashMap PackageName FilePath)
getPackagesBuilt = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ readVar packageBuilt
