module Main where

import           Control.Applicative
import           Data.Data
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import           Data.Time
import           Data.Typeable
import           System.Environment
import           System.IO

type Journal = Map.Map Day Entry
type Entry = T.Text

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
