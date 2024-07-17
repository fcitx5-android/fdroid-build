{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Config (buildDir)
import Control.Monad (forM_)
import Core
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Config (usingConfigFile)
import Development.Shake.FilePath
import Download (downloadFile)
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

packages :: [PackageDesc]
packages =
  [ PackageDesc
      { descProjectName = "fcitx5-android-plugin-pinyin-moegirl",
        descPackageName = "org.fcitx.fcitx5.android.plugin.pinyin_moegirl",
        -- TODO: manual source for testing
        descVersionSource = (Manual "20240609", Nothing), -- (GitHubRelease "outloudvi" "mw2fcitx", Nothing),
        descCreateVersionCode = read . T.unpack,
        descPreBuild = \(T.unpack -> versionName, _) projectDir ->
          downloadFile
            ("https://github.com/outloudvi/mw2fcitx/releases/download/" <> versionName <> "/moegirl.dict")
            (projectDir </> fcitx5Path </> "pinyin/dictionaries/moegirl-" <> versionName <.> "dict"),
        descAppNameDebug = "Fcitx5 for Android (moegirl dict | Debug)",
        descAppNameRelease = "Fcitx5 for Android (moegirl dict)",
        descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.moegirl.org.cn"
      }
  ]
