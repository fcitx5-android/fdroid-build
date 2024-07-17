module Download where

import Development.Shake

downloadFile :: String -> FilePath -> Action ()
downloadFile url dest = cmd_ "curl" "-L" url "-o" dest "--create-dirs"
