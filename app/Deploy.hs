module Deploy where

import Control.Monad (forM_)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath (takeFileName)
import ShakeExtras

deployRule :: Rules ()
deployRule = do
  "rsync" ~> do
    packages <- getPackagesBuilt
    if HMap.null packages
      then putInfo "No packages to deploy"
      else HMap.toList packages & forM_ $ \(pkgName, path) -> do
        putInfo $ "Pushing " <> T.unpack pkgName <> " to server"
        user <- getEnvError "DEPLOY_USER"
        host <- getEnvError "DEPLOY_HOST"
        port <- getEnvError "DEPLOY_PORT"
        keyPath <- getEnvError "DEPLOY_KEY_PATH"
        cmd_
          Shell
          "rsync"
          "-e"
          ("'ssh -o StrictHostKeyChecking=no -p " <> port <> " -i " <> keyPath <> "'")
          "-avzr"
          "--delete"
          path
          (user <> "@" <> host <> ":" <> "web/fdroid/repo/" <> takeFileName path)
  "update-repo" ~> do
    user <- getEnvError "DEPLOY_USER"
    host <- getEnvError "DEPLOY_HOST"
    port <- getEnvError "DEPLOY_PORT"
    keyPath <- getEnvError "DEPLOY_KEY_PATH"
    cmd_ Shell ("ssh -o StrictHostKeyChecking=no -p " <> port <> " -i " <> keyPath) (user <> "@" <> host) "'cd web/fdroid/ && fdroid update'"
  "deploy" ~> do
    need ["rsync"]
    need ["update-repo"]
    putInfo "Deployed"
