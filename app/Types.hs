module Types where

import Data.Text (Text)
import Development.Shake
import Nvchecker (Version, VersionSource)

type ProjectName = Text

type PackageName = Text

type VersionCode = Int

type VersionName = Text

type PackageVersion = (VersionName, VersionCode)

data PackageDesc = PackageDesc
  { descProjectName :: ProjectName,
    descPackageName :: PackageName,
    descVersionSource :: VersionSource,
    descCreateVersionCode :: Version -> VersionCode,
    descPreBuild :: FilePath -> Action (),
    descAppNameDebug :: Text,
    descAppNameRelease :: Text,
    descPluginDesc :: Text
  }
