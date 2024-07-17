module Config where

import Data.Text (Text)

data Config = Config
  { aboutlibrariesVersion :: Text,
    mainVersion :: Text,
    desugarJDKLibsVersion :: Text,
    kotlinVersion :: Text,
    androidVersion :: Text
  }
  deriving (Show)

buildDir :: FilePath
buildDir = "_build"
