module Types where

import Data.Text (Text)
import Development.Shake
import Nvchecker (NvcheckerOptions, Version, VersionSource)

type ProjectName = Text

type PackageName = Text

type VersionCode = Int

type VersionName = Text

type PackageVersion = (VersionName, VersionCode)

data PackageDesc = PackageDesc
  { descProjectName :: ProjectName,
    descPackageName :: PackageName,
    descVersionSource :: (VersionSource, Maybe NvcheckerOptions),
    descCreateVersionCode :: Version -> VersionCode,
    descPreBuild :: PackageVersion -> FilePath -> Action (),
    descAppNameDebug :: Text,
    descAppNameRelease :: Text,
    descPluginDesc :: Text
  }
