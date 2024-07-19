{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (buildDir)
import Control.Monad (forM_)
import Core
import qualified Data.HashMap.Strict as HMap
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Deploy
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
  shakeArgs
    shakeOptions
      { shakeExtra = addShakeExtra shakeExtras HMap.empty,
        shakeFiles = buildDir
      }
    $ do
      usingConfigFile "build.cfg"
      coreRule

      -- phony rules for each package
      forM_ pkgNames $ \pkgName -> phony (T.unpack pkgName) (runCore pkgName)

      "everything" ~> do
        need (T.unpack <$> pkgNames)
        need ["deploy"]

      "deploy" ~> do
        -- run after package's phony rules
        -- but don't depend on them
        orderOnly $ T.unpack <$> pkgNames
        packagesBuilt <- getPackagesBuilt
        if HMap.null packagesBuilt
          then putInfo "No packages to deploy"
          else do
            rsync
            updateRepo
            putInfo "Deployed"

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

getGitHubCommitTime :: Text -> Text -> Version -> Action Integer
getGitHubCommitTime user repo = getGitCommitTime (githubUrl user repo)

packages :: [PackageDesc]
packages =
  [ PackageDesc
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
      },
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
        descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.minecraft.org.cn"
      },
    PackageDesc
      { descProjectName = mkProjectName "pinyin-zhwiki",
        descPackageName = mkPackageName "pinyin_zhwiki",
        descVersionSource = ArchLinux "fcitx5-pinyin-zhwiki",
        -- drop converter version
        descCreateVersionName = pure . T.takeWhileEnd (/= '.'),
        -- same as version name, but in integer
        descCreateVersionCode = readInteger . T.takeWhileEnd (/= '.'),
        descPreBuild = \(version, dictVer, _) projectDir ->
          let converterVer = fromJust $ T.stripSuffix ("." <> dictVer) version
              dictName = "zhwiki-" <> T.unpack dictVer <.> "dict"
           in downloadFile
                (githubReleaseFileUrl "felixonmars" "fcitx5-pinyin-zhwiki" converterVer dictName)
                (pinyinDictPath projectDir </> dictName),
        descAppNameDebug = "Fcitx5 for Android (zhwiki dict | Debug)",
        descAppNameRelease = "Fcitx5 for Android (zhwiki dict)",
        descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.wikipedia.org"
      }
  ]
