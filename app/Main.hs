{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (buildDir)
import Control.Monad (forM_)
import Core
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Config (usingConfigFile)
import Development.Shake.FilePath
import Download (downloadFile)
import GitCommitTime
import Nvchecker
import ShakeExtras
import Types

main :: IO ()
main = do
  shakeExtras <- initShakeExtras packages
  let pkgNames = descPackageName <$> packages
  shake
    shakeOptions
      { shakeExtra = addShakeExtra shakeExtras HMap.empty,
        shakeFiles = buildDir
      }
    $ do
      usingConfigFile "build.cfg"
      coreRule
      forM_ pkgNames $ \pkgName -> phony (T.unpack pkgName) (runCore pkgName)
      "everything" ~> need (T.unpack <$> pkgNames)
      want ["everything"]

fcitx5Path :: FilePath
fcitx5Path = "app/src/main/assets/usr/share/fcitx5"

mkPackageName :: Text -> PackageName
mkPackageName name = "org.fcitx.fcitx5.android.plugin." <> name

versionNameToVersionCode :: VersionName -> Action VersionCode
versionNameToVersionCode = pure . read . T.unpack

mkProjectName :: Text -> ProjectName
mkProjectName name = "fcitx5-android-plugin-" <> name

githubUrl :: Text -> Text -> Text
githubUrl user repo = "https://github.com/" <> user <> "/" <> repo

githubReleaseFileUrl :: Text -> Text -> Text -> FilePath -> Text
githubReleaseFileUrl user repo tag file = githubUrl user repo <> "/releases/download/" <> tag <> "/" <> T.pack file

pinyinDictPath :: FilePath -> FilePath
pinyinDictPath projectDir = projectDir </> fcitx5Path </> "pinyin/dictionaries"

getGitHubCommitTime :: Text -> Text -> Version -> Action Integer
getGitHubCommitTime user repo = getGitCommitTime (githubUrl user repo)

packages :: [PackageDesc]
packages =
  [ PackageDesc
      { descProjectName = mkProjectName "pinyin-moegirl",
        descPackageName = mkPackageName "pinyin_moegirl",
        -- TODO: manual source for testing
        descVersionSource = Manual "20240609", -- (GitHubRelease "outloudvi" "mw2fcitx"),
        descCreateVersionCode = versionNameToVersionCode,
        descPreBuild = \versionName projectDir ->
          downloadFile
            (githubReleaseFileUrl "outloudvi" "mw2fcitx" versionName "moegirl.dict")
            (pinyinDictPath projectDir </> T.unpack versionName <.> "dict"),
        descAppNameDebug = "Fcitx5 for Android (moegirl dict | Debug)",
        descAppNameRelease = "Fcitx5 for Android (moegirl dict)",
        descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.moegirl.org.cn"
      },
    PackageDesc
      { descProjectName = mkProjectName "pinyin-minecraft",
        descPackageName = mkPackageName "pinyin_minecraft",
        descVersionSource = GitHubRelease "oldherl" "fcitx5-pinyin-minecraft",
        descCreateVersionCode = getGitHubCommitTime "oldherl" "fcitx5-pinyin-minecraft",
        descPreBuild = \versionName projectDir ->
          downloadFile
            (githubReleaseFileUrl "oldherl" "fcitx5-pinyin-minecraft" versionName "minecraft-cn.dict")
            (pinyinDictPath projectDir </> "minecraft-" <> T.unpack versionName <.> "dict"),
        descAppNameDebug = "Fcitx5 for Android (minecraft dict | Debug)",
        descAppNameRelease = "Fcitx5 for Android (minecraft dict)",
        descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.minecraft.org.cn"
      }
  ]
