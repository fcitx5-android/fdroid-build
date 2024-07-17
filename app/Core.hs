{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Core where

import Build
import Config (buildDir)
import Control.Monad (void)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import FDroidVersion
import GHC.Generics (Generic)
import Nvchecker
import ShakeExtras
import Singing
import Types

newtype Core = Core PackageName
  deriving (Show, Eq, Ord, Generic, Typeable, Hashable, Binary, NFData)

type instance RuleResult Core = ()

coreRule :: Rules ()
coreRule = void $ do
  nvcheckerRule
  fdroidVersionRule
  buildRule
  singingRule
  addOracle $ \(Core packageName) -> do
    PackageDesc {..} <- fromJust <$> lookupPackageDesc packageName
    putInfo $ "Checking f-droid version for " <> T.unpack descPackageName
    fdroidVersion <- getLatestFDroidVersion packageName
    putInfo $ "Checking upstream version for " <> T.unpack descPackageName
    upstreamVersion <- uncurry checkVersion $ descVersionSource
    let upstreamVersionCode = descCreateVersionCode upstreamVersion
    if fdroidVersion == Just (upstreamVersion, upstreamVersionCode)
      then do
        putInfo $ "No new version for " <> T.unpack descPackageName
        pure ()
      else do
        putInfo $ "New version for " <> T.unpack descPackageName <> ": " <> show (upstreamVersion, upstreamVersionCode)
        setPackageBuildVersion packageName (upstreamVersion, upstreamVersionCode)
        apk <- buildPackage packageName
        let signed = buildDir </> "signed" </> apk
        need [signed]

runCore :: PackageName -> Action ()
runCore packageName = askOracle $ Core packageName
