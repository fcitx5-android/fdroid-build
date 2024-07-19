module Download where

import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake

downloadFile :: Text -> FilePath -> Action ()
downloadFile url dest = cmd_ "curl" "-L" (T.unpack url) "-o" dest "--create-dirs"
