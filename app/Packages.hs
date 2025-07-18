{-# LANGUAGE OverloadedStrings #-}

module Packages (packages) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Download
import GitCommitTime
import Nvchecker
import Types

--------------------------------------------------------------------------------
packages :: [PackageDesc]
packages = [moegirl, minecraft, zhwiki, wubi98, zhengma, cangjie]

--------------------------------------------------------------------------------

fcitx5Path :: FilePath
fcitx5Path = "app/src/main/assets/usr/share/fcitx5"

mkPackageName :: Text -> PackageName
mkPackageName name = "org.fcitx.fcitx5.android.plugin." <> name

readInteger :: Text -> Action Integer
readInteger = pure . read . T.unpack

mkProjectName :: Text -> ProjectName
mkProjectName name = "fcitx5-android-plugin-" <> name

githubUrl :: Text -> Text -> Text
githubUrl user repo = "https://github.com/" <> user <> "/" <> repo

githubReleaseFileUrl :: Text -> Text -> Text -> FilePath -> Text
githubReleaseFileUrl user repo tag file = githubUrl user repo <> "/releases/download/" <> tag <> "/" <> T.pack file

pinyinDictPath :: FilePath -> FilePath
pinyinDictPath projectDir = projectDir </> fcitx5Path </> "pinyin/dictionaries"

tableIMPath :: FilePath -> FilePath
tableIMPath projectDir = projectDir </> fcitx5Path </> "inputmethod"

tableDictPath :: FilePath -> FilePath
tableDictPath projectDir = projectDir </> fcitx5Path </> "table"

getGitHubCommitTime :: Text -> Text -> Version -> Action Integer
getGitHubCommitTime user repo = getGitCommitTime (githubUrl user repo)

--------------------------------------------------------------------------------

mkTablePkg :: ProjectName -> [FilePath] -> PackageDesc
mkTablePkg name includeTables =
  PackageDesc
    { descProjectName = mkProjectName "tables-" <> name,
      descPackageName = mkPackageName "tables_" <> name,
      descVersionSource = GitHubRelease "fcitx5-android" "tables",
      descCreateVersionName = pure,
      descCreateVersionCode = readInteger,
      descPreBuild = \(version, _, _) projectDir -> forM_ includeTables $ \table -> do
        let tableZip = table <> ".zip"
            imFile = table <.> "conf"
            dictFile = table <.> "main" <.> "dict"
        downloadFile (githubReleaseFileUrl "fcitx5-android" "tables" version tableZip) (projectDir </> tableZip)
        cmd_ (Cwd projectDir) ("unzip" :: String) tableZip
        copyFile' (projectDir </> imFile) (tableIMPath projectDir </> imFile)
        copyFile' (projectDir </> dictFile) (tableDictPath projectDir </> dictFile),
      descAppNameDebug = "Fcitx5 for Android (tables " <> name <> " | Debug)",
      descAppNameRelease = "Fcitx5 for Android (tables " <> name <> ")",
      descPluginDesc = "Fcitx 5 tables " <> name
    }

--------------------------------------------------------------------------------

moegirl :: PackageDesc
moegirl =
  PackageDesc
    { descProjectName = mkProjectName "pinyin-moegirl",
      descPackageName = mkPackageName "pinyin_moegirl",
      descVersionSource = GitHubRelease "outloudvi" "mw2fcitx",
      descCreateVersionName = pure,
      descCreateVersionCode = readInteger,
      descPreBuild = \(_, dictVer, _) projectDir ->
        downloadFile
          (githubReleaseFileUrl "outloudvi" "mw2fcitx" dictVer "moegirl.dict")
          (pinyinDictPath projectDir </> T.unpack dictVer <.> "dict"),
      descAppNameDebug = "Fcitx5 for Android (moegirl dict | Debug)",
      descAppNameRelease = "Fcitx5 for Android (moegirl dict)",
      descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.moegirl.org.cn"
    }

minecraft :: PackageDesc
minecraft =
  PackageDesc
    { descProjectName = mkProjectName "pinyin-minecraft",
      descPackageName = mkPackageName "pinyin_minecraft",
      descVersionSource = GitHubRelease "oldherl" "fcitx5-pinyin-minecraft",
      descCreateVersionName = pure,
      descCreateVersionCode = getGitHubCommitTime "oldherl" "fcitx5-pinyin-minecraft",
      descPreBuild = \(_, dictVer, _) projectDir ->
        downloadFile
          (githubReleaseFileUrl "oldherl" "fcitx5-pinyin-minecraft" dictVer "minecraft-cn.dict")
          (pinyinDictPath projectDir </> "minecraft-" <> T.unpack dictVer <.> "dict"),
      descAppNameDebug = "Fcitx5 for Android (minecraft dict | Debug)",
      descAppNameRelease = "Fcitx5 for Android (minecraft dict)",
      descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.minecraft.wiki"
    }

zhwiki :: PackageDesc
zhwiki =
  PackageDesc
    { descProjectName = mkProjectName "pinyin-zhwiki",
      descPackageName = mkPackageName "pinyin_zhwiki",
      descVersionSource = ArchLinux "fcitx5-pinyin-zhwiki",
      -- drop converter version
      descCreateVersionName = pure . T.takeWhileEnd (/= '.'),
      -- same as version name, but in integer
      descCreateVersionCode = readInteger . T.takeWhileEnd (/= '.'),
      descPreBuild = \(version, dictVer, _) projectDir ->
        let converterVer = case T.stripSuffix ("." <> dictVer) version of
              Just v -> v
              Nothing -> error $ "Invalid version " <> T.unpack version
            dictName = "zhwiki-" <> T.unpack dictVer <.> "dict"
         in downloadFile
              (githubReleaseFileUrl "felixonmars" "fcitx5-pinyin-zhwiki" converterVer dictName)
              (pinyinDictPath projectDir </> dictName),
      descAppNameDebug = "Fcitx5 for Android (zhwiki dict | Debug)",
      descAppNameRelease = "Fcitx5 for Android (zhwiki dict)",
      descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.wikipedia.org"
    }

wubi98 :: PackageDesc
wubi98 = mkTablePkg "wubi98" ["wubi98-pinyin", "wubi98-single", "wubi98", "wubi98-large"]

zhengma :: PackageDesc
zhengma = mkTablePkg "zhengma" ["zhengma", "zhengma-large", "zhengma-pinyin"]

cangjie :: PackageDesc
cangjie = mkTablePkg "cangjie" ["cangjie5", "cangjie3", "cangjie-large", "scj6"]

--------------------------------------------------------------------------------
