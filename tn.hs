{-# LANGUAGE OverloadedStrings #-}

-- tn - a simple journal program
-- Copyright (c) 2014-2015, Peter Harpending.
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in
--    the documentation and/or other materials provided with the
--    distribution.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
-- BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
-- OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
-- AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
-- WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.


-- | 
-- Module      : Main
-- Description : Runs tn
-- Copyright   : Copyright (c) 2014-2015, Peter Harpending.
-- License     : FreeBSD
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC

module Main where

import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Combinators (sinkLazy)
import Data.Conduit.Text (decodeUtf8)
import Data.HashMap.Lazy (HashMap, insert, lookup, fromList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Time (getCurrentTime)
import Prelude hiding (lookup)
import System.Directory
import System.Exit
import System.IO (stdout)
import Text.Editor

type Journal = HashMap T.Text L.Text
type Journals = HashMap T.Text Journal

initialize :: IO ()
initialize =
  do jp <- journalsPath
     runResourceT
       (connect (sourceLbs (encode ((fromList [("default",mempty)]) :: Journals)))
                (sinkFile jp))

-- |Adapted from Data.Yaml.
parseMonad :: (Monad m, FromJSON a) => Value -> m a
parseMonad v =
  case fromJSON v of
    Success a -> pure a
    Error s -> fail s

-- |The path to the user's 
journalsPath :: IO FilePath
journalsPath =
  do dataDir <- getAppUserDataDirectory "tn"
     pure (mappend dataDir "/journals.json")

-- |Read the journals
readJournals :: IO Journals
readJournals =
  do jp <- journalsPath
     parseMonad =<<
       (runResourceT
          (connect (sourceFile jp)
                   (sinkParser json)))

writeJournals :: Journals -> IO ()
writeJournals journals =
  do jp <- journalsPath
     runResourceT
       (connect (sourceLbs (encode journals))
                (sinkFile jp))

defaultJournal :: IO Journal
defaultJournal =
  do journals <- readJournals
     case lookup "default" journals of
       Nothing ->
         do jp <- journalsPath
            fail (mappend "Could not find journal \"default\" in " jp)
       Just j -> pure j

addEntry :: Journal -> IO Journal
addEntry journal =
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
         pure (insert (T.pack (show currentTime)) entry journal)

ppJournal :: Journal -> IO ()
ppJournal journal =
  runResourceT
    (connect (sourceLbs (mappend (encodePretty journal) "\n"))
             (sinkHandle stdout))

main :: IO ()
main = pure ()
