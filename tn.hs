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
import Options.Applicative hiding (Success, action)
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
    
alt :: Alternative f => [f a] -> f a
alt [] = empty
alt (x : xs) = x <|> alt xs

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
defaultJournal = getJournal "default"

getJournal :: T.Text -> IO Journal
getJournal s =
  do journals <- readJournals
     case lookup s journals of
       Nothing ->
         do jp <- journalsPath
            fail (mappend "Could not find journal \"default\" in " jp)
       Just j -> pure j

addEntryTo :: Journal -> IO Journal
addEntryTo journal =
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

addEntry :: T.Text {- |Journal name -} -> IO ()
addEntry journalName =
  do journal <- getJournal journalName
     newJournal <- addEntryTo journal
     journals <- readJournals
     let newJournals =
           insert journalName newJournal journals
     writeJournals newJournals

ppJournal :: T.Text -> IO ()
ppJournal journalName =
  do journal <- getJournal journalName
     runResourceT
       (connect (sourceLbs (mappend (encodePretty journal) "\n"))
                (sinkHandle stdout))

data Args =
  Args {argsJournal :: String
       ,argsAction :: Action}
  deriving (Show)
data Action
  = Entry EntryAction
  | Journal JournalAction
  deriving (Show)
            
data EntryAction
  = Add
  | Edit
  deriving (Show)

data JournalAction
  = List
  | PrettyPrint
  deriving (Show)

argsParser :: ParserInfo Args
argsParser =
  info (helper <*>
        ((pure Args) <*>
         (strOption (mconcat [short 'j'
                             ,long "journal"
                             ,help "Journal on which to operate. See `tn lj` for a list."
                             ,metavar "NAME"
                             ,value "default"])) <*>
         (alt [subparser (command "ae" (info empty mempty))
              ,subparser (command "ee" (info empty mempty))
              ,subparser (command "lj" (info empty mempty))
              ,subparser (command "pp" (info empty mempty))])))
       (mconcat [fullDesc,progDesc "A simple journal program"])

runArgs :: Args -> IO ()
runArgs (Args journalName action) =
  let journalNameText = T.pack journalName
  in case action of
       (Entry Add) -> addEntry journalNameText
       (Entry Edit) ->
         fail "FIXME: Not implemented"
       (Journal PrettyPrint) -> ppJournal journalNameText
       (Journal List) ->
         fail "FIXME: Not implemented"

mainWith :: [String] -> Maybe Args
mainWith =
  getParseResult .
  execParserPure parserPrefs argsParser

parserPrefs :: ParserPrefs
parserPrefs =
  prefs (mconcat [disambiguate,showHelpOnError])

main :: IO ()
main = customExecParser parserPrefs argsParser >>= print
