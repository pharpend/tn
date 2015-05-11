-- tn - a simple journal program
-- Copyright (C) 2015 Peter Harpending
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at
-- your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Main
-- Description : Runs tn
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This runs @tn@. This module isn't very interesting, it just
-- processes command line arguments, and then sends the relevant
-- information to "Tn.Meat".

module Main where

import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Combinators (sinkLazy, print)
import Data.Conduit.Text (decodeUtf8)
import Data.Map (Map, insert)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time (UTCTime, getCurrentTime)
import Prelude hiding (print)
import System.Directory
import System.Exit
import Text.Editor

type Journal = Map T.Text L.Text

parseMonad :: (Monad m, FromJSON a) => Value -> m a
parseMonad v =
  case fromJSON v of
    Success a -> pure a
    Error s -> fail s

journalPath :: IO FilePath
journalPath =
  do dataDir <- getAppUserDataDirectory "tn"
     pure (mappend dataDir "/journal.json")

readJournal :: IO Journal
readJournal =
  do jp <- journalPath
     parseMonad =<<
       (runResourceT
          (connect (sourceFile jp)
                   (sinkParser json)))

writeJournal :: Journal -> IO ()
writeJournal j =
  do jp <- journalPath
     runResourceT
       (connect (sourceLbs (encode j))
                (sinkFile jp))

addEntry :: IO ()
addEntry =
  do currentTime <- getCurrentTime
     (exitCode,entry) <-
       runResourceT
         (bracketConduit plainTemplate
                         (toProducer (sourceLbs mempty))
                         (toConsumer (fuse decodeUtf8 sinkLazy)))
     case exitCode of
       a@(ExitFailure _) ->
         fail (mconcat ["Editor failed with ",show a,"."])
       ExitSuccess ->
         do journal <- readJournal
            let newJournal =
                  insert (T.pack (show currentTime)) entry journal
            writeJournal newJournal

ppJournal :: IO ()
ppJournal =
  do journal <- readJournal
     runResourceT
       (connect (sourceLbs (encodePretty journal))
                print)


main :: IO ()
main = return ()
