{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config (buildDir)
import Control.Monad (forM_)
import Core
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text)
import qualified Data.Text as T
import Deploy
import Development.Shake
import Development.Shake.Config (usingConfigFile)
import Development.Shake.FilePath
import Packages
import ShakeExtras
import System.Console.GetOpt
import Types

--------------------------------------------------------------------------------
data Flags = FlagSkipFdroidVersionCheck
  deriving (Show, Eq)

flags :: [OptDescr (Either a Flags)]
flags = [Option "s" ["skip-fdroid"] (NoArg $ Right FlagSkipFdroidVersionCheck) "Skip checking f-droid version"]

main :: IO ()
main = shakeArgsOptionsWith shakeOptions {shakeFiles = buildDir} flags $ \shakeOptions1 parsedFlags targets -> do
  shakeExtras <- initShakeExtras packages $ FlagSkipFdroidVersionCheck `notElem` parsedFlags
  let pkgNames = descPackageName <$> packages
      shakeOptions2 = shakeOptions1 {shakeExtra = addShakeExtra shakeExtras HMap.empty}
      rules = do
        usingConfigFile "build.cfg"
        coreRule

        -- phony rules for each package
        forM_ pkgNames $ \pkgName -> phony (T.unpack pkgName) (runCore pkgName)

        "build" ~> do
          need (T.unpack <$> pkgNames)
          changelog <- T.unpack <$> generateChangelog
          if null changelog
            then putInfo "No changelog"
            else do
              putInfo changelog
              writeFile' (buildDir </> "changelog.txt") changelog

        "deploy" ~> do
          need ["build"]
          packagesBuilt <- getPackagesBuilt
          if HMap.null packagesBuilt
            then putInfo "No packages to deploy"
            else do
              rsync
              updateRepo
              putInfo "Deployed"
              writeFile' (buildDir </> "deployed") ""

        "update-repo" ~> updateRepo

        "clean" ~> do
          removeFilesAfter buildDir ["//*"]
          putInfo "Cleaned"
  pure $ Just (shakeOptions2, if null targets then rules else want targets >> withoutActions rules)

--------------------------------------------------------------------------------

generateChangelog :: Action Text
generateChangelog =
  HMap.foldlWithKey'
    ( \acc pkgName (fdroidVersion, (_, newVersionName, newVersionCode)) ->
        acc
          <> pkgName
          <> ": "
          <> ( case fdroidVersion of
                 Just (oldVersionName, oldVersionCode) -> oldVersionName <> " (" <> showVersionCode oldVersionCode <> ")"
                 Nothing -> "∅"
             )
          <> " → "
          <> newVersionName
          <> " ("
          <> showVersionCode newVersionCode
          <> ")"
          <> "\n"
    )
    ""
    <$> getAllChangeLogs
