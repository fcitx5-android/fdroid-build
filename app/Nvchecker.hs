{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Adapted from https://github.com/berberman/nvfetcher/blob/master/src/NvFetcher/Nvchecker.hs
module Nvchecker
  ( Version,
    VersionSortMethod (..),
    ListOptions (..),
    CheckVersion (..),
    NvcheckerOptions (..),
    VersionSource (..),
    nvcheckerRule,
    checkVersion,
  )
where

import Control.Monad (void)
import Control.Monad.Trans.Writer
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics

type Version = Text

data VersionSortMethod = ParseVersion | Vercmp
  deriving (Typeable, Eq, Ord, Enum, Generic, Hashable, Binary, NFData)

instance Show VersionSortMethod where
  show = \case
    ParseVersion -> "parse_version"
    Vercmp -> "vercmp"

data ListOptions = ListOptions
  { _includeRegex :: Maybe Text,
    _excludeRegex :: Maybe Text,
    _sortVersionKey :: Maybe VersionSortMethod,
    _ignored :: Maybe Text
  }
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

data NvcheckerOptions = NvcheckerOptions
  { _stripPrefix :: Maybe Text,
    _fromPattern :: Maybe Text,
    _toPattern :: Maybe Text
  }
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

-- | Upstream version source for nvchecker to check
data VersionSource
  = GitHubRelease {_owner :: Text, _repo :: Text}
  | GitHubTag {_owner :: Text, _repo :: Text, _listOptions :: ListOptions}
  | Git {_vurl :: Text, _vbranch :: Maybe Text}
  | ArchLinux {_archpkg :: Text}
  | Aur {_aur :: Text}
  | Manual {_manual :: Text}
  | Repology {_repology :: Text, _repo :: Text}
  | Webpage {_vurl :: Text, _regex :: Text, _listOptions :: ListOptions}
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

data CheckVersion = CheckVersion VersionSource NvcheckerOptions
  deriving (Show, Typeable, Eq, Ord, Generic, Hashable, Binary, NFData)

data NvcheckerRaw = NvcheckerSuccess Text | NvcheckerError Text
  deriving (Show, Typeable, Eq, Generic)

instance A.FromJSON NvcheckerRaw where
  parseJSON = A.withObject "NvcheckerRaw" $ \o -> do
    mVersion <- o A..:? "version"
    case mVersion of
      Just version -> pure $ NvcheckerSuccess version
      _ -> NvcheckerError <$> o A..: "error"

type instance RuleResult CheckVersion = Version

nvcheckerRule :: Rules ()
nvcheckerRule = void $
  addOracle $ \(CheckVersion versionSource options) ->
    runNvchecker options versionSource

runNvchecker :: NvcheckerOptions -> VersionSource -> Action Text
runNvchecker options versionSource = withTempFile $ \config -> do
  let nvcheckerConfig = T.unpack $ T.unlines $ execWriter $ genNvConfig options versionSource
  putVerbose $ "Generated nvchecker config: " <> nvcheckerConfig
  writeFile' config nvcheckerConfig
  (CmdTime t, Stdout out, CmdLine c) <- quietly . cmd $ "nvchecker --logger json -c " <> config
  putVerbose $ "Finishing running " <> c <> ", took " <> show t <> "s"
  case reverse . lines $ out of
    (o : _) | Just raw <- A.decodeStrict' $ BS.pack o -> case raw of
      NvcheckerSuccess x -> pure x
      NvcheckerError err -> fail $ "Failed to run nvchecker: " <> T.unpack err
    _ -> fail $ "Failed to parse output from nvchecker: " <> out

type BuildTOML = Writer [Text] ()

genNvConfig :: NvcheckerOptions -> VersionSource -> BuildTOML
genNvConfig options versionSource =
  table
    ( do
        genVersionSource versionSource
        genOptions options
    )
  where
    key =: x = tell [key <> " = " <> "\"" <> x <> "\""]
    key =:? (Just x) = key =: x
    _ =:? _ = pure ()
    table m = tell ["[pkg]"] >> m >> tell [""]
    genVersionSource = \case
      GitHubRelease {..} -> do
        "source" =: "github"
        "github" =: (_owner <> "/" <> _repo)
        "use_latest_release" =: "true"
      GitHubTag {..} -> do
        "source" =: "github"
        "github" =: (_owner <> "/" <> _repo)
        "use_max_tag" =: "true"
        genListOptions _listOptions
      Git {..} -> do
        "source" =: "git"
        "git" =: _vurl
        "branch" =:? _vbranch
        "use_commit" =: "true"
      Aur {..} -> do
        "source" =: "aur"
        "aur" =: _aur
        "strip_release" =: "true"
      ArchLinux {..} -> do
        "source" =: "archpkg"
        "archpkg" =: _archpkg
        "strip_release" =: "true"
      Manual {..} -> do
        "source" =: "manual"
        "manual" =: _manual
      Repology {..} -> do
        "source" =: "repology"
        "repology" =: _repology
        "repo" =: _repo
      Webpage {..} -> do
        "source" =: "regex"
        "url" =: _vurl
        "regex" =: _regex
        genListOptions _listOptions
    genListOptions ListOptions {..} = do
      "include_regex" =:? _includeRegex
      "exclude_regex" =:? _excludeRegex
      "sort_version_key" =:? fmap (T.pack . show) _sortVersionKey
      "ignored" =:? _ignored
    genOptions NvcheckerOptions {..} = do
      "prefix" =:? _stripPrefix
      "from_pattern" =:? _fromPattern
      "to_pattern" =:? _toPattern

checkVersion :: VersionSource -> Maybe NvcheckerOptions -> Action Version
checkVersion v o = askOracle $ CheckVersion v $ fromMaybe (NvcheckerOptions mempty mempty mempty) o
