module Sign where

import Config
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Char8 as BS
import Development.Shake
import Development.Shake.FilePath
import System.Directory.Extra (createDirectoryIfMissing)

singRule :: Rules ()
singRule = do
  buildDir </> "signed" </> "*.apk" %> \out -> do
    liftIO $ createDirectoryIfMissing True (buildDir </> "signed")
    let src = buildDir </> "unsigned" </> takeFileName out
    putInfo $ "Signing " <> src
    need [src]
    buildToolsRoot <- getEnvError "ANDROID_BUILD_TOOLS_ROOT"
    let zipalign = buildToolsRoot </> "zipalign"
        apksigner = buildToolsRoot </> "apksigner"
    withTempDir $ \dir -> do
      let aligned = dir </> "aligned.apk"
      cmd_ zipalign "-p" "4" src aligned
      withTempFile $ \keyFile -> do
        signKey <- getEnvError "SIGN_KEY_BASE64"
        case BS64.decode $ BS.pack signKey of
          Left err -> fail err
          Right key -> liftIO $ BS.writeFile keyFile key
        keyAlias <- getEnvError "SIGN_KEY_ALIAS"
        need [aligned]
        cmd_ apksigner "sign" "--out" out "--ks" keyFile "--ks-pass" "env:SIGN_KEY_PWD" "--ks-key-alias" keyAlias aligned
