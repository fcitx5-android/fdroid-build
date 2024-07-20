module Deploy where

import Control.Monad (forM_)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath (takeFileName)
import ShakeExtras

rsync :: Action ()
rsync = do
  packages <- getPackagesBuilt
  HMap.toList packages & forM_ $ \(pkgName, path) -> do
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

updateRepo :: Action ()
updateRepo = do
  user <- getEnvError "DEPLOY_USER"
  host <- getEnvError "DEPLOY_HOST"
  port <- getEnvError "DEPLOY_PORT"
  keyPath <- getEnvError "DEPLOY_KEY_PATH"
  cmd_ Shell ("ssh -o StrictHostKeyChecking=no -p " <> port <> " -i " <> keyPath) (user <> "@" <> host) "'cd web/fdroid/ && fdroid update'"