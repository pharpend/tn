module Main where

import qualified Data.Text as T
import           Data.Time

data Journal = Journal { date :: UTCTime
                       , text :: T.Text
                       }

main :: IO ()
main = return ()
