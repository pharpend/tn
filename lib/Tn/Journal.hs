-- |Tn Journals
module Tn.Journal where

import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Yaml
import           GHC.Generics
import           System.IO
import           System.Pager (printOrPage)

type Journal = Vector Entry

data Entry = Entry { entryTime :: UTCTime
                   , entryText :: Text
                   }
  deriving (Eq, Show, Generic)

instance ToJSON Entry
instance FromJSON Entry

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

-- |Print a journal
printJournal :: Journal         -- ^Journal to print
             -> Bool            -- ^Whether or not to enable use of a pager.
             -> IO ()
printJournal j p
  | p = printOrPage $ T.decodeUtf8 (encode j)
  | otherwise = B.hPut stdout (encode j)
