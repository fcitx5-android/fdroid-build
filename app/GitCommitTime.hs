{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module GitCommitTime where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)

data GitCommitTime = GitCommitTime {gitUrl :: Text, gitRev :: Text}
  deriving (Show, Eq, Ord, Hashable, NFData, Binary, Typeable, Generic)

type instance RuleResult GitCommitTime = Integer

-- | Timestamp in seconds since epoch
gitCommitTimeRule :: Rules ()
gitCommitTimeRule = void $ do
  addOracleCache $ \(GitCommitTime (T.unpack -> url) (T.unpack -> rev)) -> withTempDir $ \repo -> do
    (StdoutTrim out) <- quietly $ do
      cmd_ [Cwd repo, EchoStderr False, EchoStdout False] "git init"
      cmd_ [Cwd repo, EchoStderr False] $ "git remote add origin " <> url
      cmd_ [Cwd repo, EchoStderr False] $ "git fetch --depth 1 origin " <> rev
      cmd_ [Cwd repo, EchoStderr False] ("git checkout FETCH_HEAD" :: String)
      cmd [Cwd repo, Shell] "git --no-pager log -1 --format=%cd --date=format:%s"
    pure $ read out

getGitCommitTime :: Text -> Text -> Action Integer
getGitCommitTime url rev = askOracle $ GitCommitTime url rev
