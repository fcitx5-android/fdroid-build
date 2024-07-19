{-# LANGUAGE ExistentialQuantification #-}

module Types where

import Data.Text (Text)
import Development.Shake
import Nvchecker

type ProjectName = Text

type PackageName = Text

type VersionCode = Integer

type VersionName = Text

type PackageVersion = (VersionName, VersionCode)

data PackageDesc = forall nv.
  (ToNvcheckerInput nv) =>
  PackageDesc
  { descProjectName :: ProjectName,
    descPackageName :: PackageName,
    descVersionSource :: nv,
    descCreateVersionCode :: Version -> Action VersionCode,
    descPreBuild :: VersionName -> FilePath -> Action (),
    descAppNameDebug :: Text,
    descAppNameRelease :: Text,
    descPluginDesc :: Text
  }
