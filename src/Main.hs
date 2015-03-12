module Main where

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Data
import qualified Data.Map.Lazy as Map
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time
import           Data.Typeable
import           Data.Yaml
import           Paths_tn (version)
import           Safe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.IO.Temp
import           System.Process

type Journal = Map.Map Day Entry
type Entry = T.Text

thisApp :: String
thisApp = "tn"

today :: IO Day
today = utctDay <$> getCurrentTime

todayMinus :: Integer -> IO Day
todayMinus i = addDays (-1 * i) <$> today

main :: IO ()
main = print =<< myJournal
  where
    myJournal :: IO Journal
    myJournal = do
      tdy <- today
      ytdy <- todayMinus 1
      return $ Map.fromList [(tdy, "Yay trivial entry"), (ytdy, "Yay another trivial entry")]
