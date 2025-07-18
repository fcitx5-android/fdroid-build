{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Core where

import Build
import Config
import Control.Monad (join, void)
import Control.Monad.Extra (whenMaybe)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import FDroidVersion
import GHC.Generics (Generic)
import GitCommitTime
import Nvchecker
import ShakeExtras
import Sign
import Types

newtype Core = Core PackageName
  deriving (Show, Eq, Ord, Generic, Typeable, Hashable, Binary, NFData)

type instance RuleResult Core = ()

coreRule :: Rules ()
coreRule = void $ do
  nvcheckerRule
  fdroidVersionRule
  buildRule
  signRule
  gitCommitTimeRule
  addOracle $ \(Core packageName) -> do
    PackageDesc {..} <- getPackageDesc packageName
    checkFdroidVersion <- getCheckFdroidVersion
    fdroidVersion <- fmap join $ whenMaybe checkFdroidVersion $ do
      putInfo $ "Checking f-droid version for " <> T.unpack descPackageName
      getLatestFDroidVersion packageName
    putInfo $ "Checking upstream version for " <> T.unpack descPackageName
    upstreamVersion <- checkVersion descVersionSource
    upstreamVersionName <- descCreateVersionName upstreamVersion
    upstreamVersionCode <- descCreateVersionCode upstreamVersion
    let ver = (upstreamVersion, upstreamVersionName, upstreamVersionCode)
    if checkFdroidVersion && (fdroidVersion == Just (upstreamVersionName, upstreamVersionCode))
      then do
        putInfo $ "No new version for " <> T.unpack descPackageName
        pure ()
      else do
        putInfo $ "New version for " <> T.unpack descPackageName <> " (" <> T.unpack upstreamVersion <> ")" <> ": " <> show fdroidVersion <> " -> " <> show (upstreamVersionName, upstreamVersionCode)
        apk <- buildPackage packageName ver
        let signed = buildDir </> "signed" </> apk
        need [signed]
        putInfo $ "Built " <> T.unpack packageName <> " at " <> signed
        markPackageBuilt packageName signed
        addChangelog packageName fdroidVersion ver

runCore :: PackageName -> Action ()
runCore packageName = askOracle $ Core packageName
