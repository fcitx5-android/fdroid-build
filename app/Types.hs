{-# LANGUAGE ExistentialQuantification #-}

module Types where

import Data.Text (Text)
import Development.Shake
import Nvchecker

type ProjectName = Text

type PackageName = Text

type VersionCode = Integer

type VersionName = Text

type PackageVersion = (Version, VersionName, VersionCode)

data PackageDesc = forall nv.
  (ToNvcheckerInput nv) =>
  PackageDesc
  { descProjectName :: ProjectName,
    descPackageName :: PackageName,
    descVersionSource :: nv,
    descCreateVersionName :: Version -> Action VersionName,
    descCreateVersionCode :: Version -> Action VersionCode,
    descPreBuild :: PackageVersion -> FilePath -> Action (),
    descAppNameDebug :: Text,
    descAppNameRelease :: Text,
    descPluginDesc :: Text
  }
