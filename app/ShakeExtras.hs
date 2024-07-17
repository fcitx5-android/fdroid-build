{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module ShakeExtras where

import Config
import Control.Concurrent.Extra
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Maybe (fromJust)
import Development.Shake
import Network.HTTP.Client (Manager)
import Types

data ShakeExtras = ShakeExtras
  { httpManager :: Manager,
    packageDesc :: HashMap PackageName PackageDesc,
    packageVersion :: Var (HashMap PackageName PackageVersion),
    config :: Config
  }

getShakeExtras :: Action ShakeExtras
getShakeExtras = fromJust <$> getShakeExtra @ShakeExtras

lookupPackageDesc :: PackageName -> Action (Maybe PackageDesc)
lookupPackageDesc pkgName = do
  ShakeExtras {..} <- getShakeExtras
  pure $ HMap.lookup pkgName packageDesc

setPackageVersion :: PackageName -> PackageVersion -> Action ()
setPackageVersion pkgName pkgVersion = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ modifyVar_ packageVersion $ pure . HMap.insert pkgName pkgVersion

getPackageVersion :: PackageName -> Action (Maybe PackageVersion)
getPackageVersion pkgName = do
  ShakeExtras {..} <- getShakeExtras
  liftIO $ HMap.lookup pkgName <$> readVar packageVersion

getHttpManager :: Action Manager
getHttpManager = httpManager <$> getShakeExtras

getConfig :: Action Config
getConfig = config <$> getShakeExtras
