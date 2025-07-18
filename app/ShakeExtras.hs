{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module ShakeExtras where

import Control.Concurrent.Extra
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Development.Shake
import FDroidVersion (FDroidVersion)
import Types

data ShakeExtras = ShakeExtras
  { packageDesc :: HashMap PackageName PackageDesc,
    packageBuilt :: Var (HashMap PackageName FilePath),
    packageChangelog :: Var (HashMap PackageName (Maybe FDroidVersion, PackageVersion)),
    checkFdroidVersion :: Bool
  }

initShakeExtras :: [PackageDesc] -> Bool -> IO ShakeExtras
initShakeExtras pkgs checkFdroidVersion = do
  packageBuilt <- newVar HMap.empty
  packageChangelog <- newVar HMap.empty
  let packageDesc = HMap.fromList $ map (\p -> (descPackageName p, p)) pkgs
  pure $ ShakeExtras {..}

getShakeExtras :: Action ShakeExtras
getShakeExtras =
  getShakeExtra @ShakeExtras >>= \case
    Just v -> pure v
    Nothing -> fail "ShakeExtras not found"

getCheckFdroidVersion :: Action Bool
getCheckFdroidVersion = do
  ShakeExtras {..} <- getShakeExtras
  pure checkFdroidVersion

getPackageDesc :: PackageName -> Action PackageDesc
getPackageDesc pkgName = do
  ShakeExtras {..} <- getShakeExtras
  liftIO (pure (HMap.lookup pkgName packageDesc)) >>= \case
    Just v -> pure v
    Nothing -> fail $ "Package " <> show pkgName <> " not found in packageDesc"

markPackageBuilt :: PackageName -> FilePath -> Action ()
markPackageBuilt pkgName path = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ modifyVar_ packageBuilt $ pure . HMap.insert pkgName path

getPackagesBuilt :: Action (HashMap PackageName FilePath)
getPackagesBuilt = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ readVar packageBuilt

addChangelog :: PackageName -> Maybe FDroidVersion -> PackageVersion -> Action ()
addChangelog pkgName fdroidVersion version = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ modifyVar_ packageChangelog $ pure . HMap.insert pkgName (fdroidVersion, version)

getAllChangeLogs :: Action (HashMap PackageName (Maybe FDroidVersion, PackageVersion))
getAllChangeLogs = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ readVar packageChangelog
