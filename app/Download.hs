module Download where

import Development.Shake

downloadFile :: String -> FilePath -> Action ()
downloadFile url = cmd_ "curl" "-L" url "-o"
