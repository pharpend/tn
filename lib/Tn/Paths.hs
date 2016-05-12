module Tn.Paths where

import Tn.Errata

import qualified Data.Text.IO as T
import           System.Directory

-- |The directory in which we're storing our data.
-- 
-- On *nix, this is something like
-- 
-- > ~/.local/share/tn
-- 
-- This can be overridden with certain environment variables. See the
-- module documentation in "System.Directory" for more details.
dataPath :: IO FilePath
dataPath = getXdgDirectory XdgData appName

-- |The path to the journal
-- 
-- This will be 'dataPath' plus @"/journal.yaml"@
journalPath :: IO FilePath
journalPath =
  do dp <- dataPath
     return $ mappend dp "/journal.yaml"

-- |Create the needed paths, and put a skeleton in 'journalPath' if need
-- be.
createNeededFiles :: IO ()
createNeededFiles =
  do dp <- dataPath
     jp <- journalPath
     createDirectoryIfMissing True dp
     jpExists <- doesFileExist jp
     if jpExists
       then return ()
       else T.writeFile jp "[]"
