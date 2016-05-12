-- |Tn Journals
module Tn.Journal where

import           Tn.Paths

import qualified Data.ByteString as B
import           Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.IO
import           System.Pager (printOrPage)

type Journal = Vector Entry

data Entry = Entry { entryTime :: UTCTime
                   , entryText :: Text
                   }
  deriving (Eq, Show, Generic)

instance ToJSON Entry
instance FromJSON Entry

-- |Make an entry, using the current Time
mkEntry :: String -> IO Entry
mkEntry s =
  Entry <$> getCurrentTime
        <*> pure (pack s)

-- |Add an entry to a journal
-- 
-- Actually just a clever alias for 'V.snoc'
addEntry :: Journal -> Entry -> Journal
addEntry = V.snoc

-- |Read a journal from stdin
readStdin :: IO (Either ParseException Journal)
readStdin =
  do contents <- B.hGetContents stdin
     return $ decodeEither' contents

-- |Read a journal from stdin, failing if the parse fails
readStdin' :: IO Journal
readStdin' =
  readStdin
  >>= \case
        Left x -> fail (show x)
        Right x ->  return x

-- |Read a journal from a file
readJournalFile :: FilePath -> IO Journal
readJournalFile fp =
  do fp' <- makeAbsolute fp
     res <- decodeFileEither fp'
     case res of
       Left x -> fail (show x)
       Right x ->  return x

-- |Read a journal from normal file
readJournal :: IO Journal
readJournal =
  do createNeededFiles
     jp <- journalPath
     readJournalFile jp

-- |Print a journal
printJournal :: Journal         -- ^Journal to print
             -> Bool            -- ^Whether or not to enable use of a pager.
             -> IO ()
printJournal j p
  | p = printOrPage $ T.decodeUtf8 (encode j)
  | otherwise = B.hPut stdout (encode j)

-- |Write a journal to a file
writeJournalFile :: Journal -> FilePath -> IO ()
writeJournalFile j f = encodeFile f j

-- |Read a journal from normal file
writeJournal :: Journal -> IO ()
writeJournal j =
  do createNeededFiles
     jp <- journalPath
     writeJournalFile j jp
