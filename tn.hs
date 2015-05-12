{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Monad (forM_, mzero)
import Control.Monad.Trans.Resource
import Crypto.Random
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Bs16
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Conduit.Combinators (sinkLazy)
import Data.Conduit.Text (decodeUtf8)
import Data.HashMap.Lazy (HashMap, insert, lookup, fromList)
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Text.IO (putStrLn)
import qualified Data.Text.Lazy as L
import Data.Time (UTCTime, getCurrentTime)
import Options.Applicative hiding (Success, action)
import Prelude hiding (lookup, putStrLn)
import System.Directory
import System.Exit
import System.IO (stdout)
import Text.Editor

data Entry = Entry UTCTime L.Text
  deriving (Eq, Show)
type Journal = HashMap T.Text Entry
type Journals = HashMap T.Text Journal

instance FromJSON Entry where
  parseJSON (Object v) = Entry <$> v .: "time" <*> v .: "text"
  parseJSON _ = mzero
  
instance ToJSON Entry where
  toJSON (Entry time_ text_) =
    object ["time" .= time_,"text" .= text_]

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
     entryText <-
       fmap (L.fromStrict . E.decodeUtf8)
            (runUserEditorDWIM plainTemplate mempty)
     id <- randomIdentFor journal
     pure (insert id (Entry currentTime entryText) journal)
  where randomIdentFor j =
          do ident <-
               fmap E.decodeUtf8 (randomIdent 4)
             case lookup ident journal of
               -- On the off chance the ident is already in the journal, try again.
               Just _ -> randomIdentFor j
               Nothing -> pure ident
        randomIdent length =
          fmap cprgCreate createEntropyPool >>=
          \(rng :: SystemRNG) ->
            let (bs,_) = cprgGenerate length rng
            in pure (Bs16.encode bs)

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
                
listJournals :: IO ()
listJournals =
  do journalNames <- fmap M.keys readJournals
     putStrLn (T.unwords journalNames)


data Args
  = DoEntry String
            EntryAction
  | Journal JournalAction
  deriving (Show)
            
data EntryAction
  = Add
  | Edit String
  deriving (Show)

data JournalAction
  = List
  | PrettyPrint String
  deriving (Show)

argsParserInfo :: ParserInfo Args
argsParserInfo =
  info (helper <*> argsParser)
       (mconcat [fullDesc,progDesc "A simple journal program",tnHeader,tnFooter])

argsParser :: Parser Args
argsParser =
  (alt [subparser (command "journal"
                           (info (helper <*> journalArgParser)
                                 (mconcat [briefDesc
                                          ,progDesc "Do stuff with journals"
                                          ,tnHeader
                                          ,tnFooter])))
       ,subparser (command "entry"
                           (info (helper <*> entryArgParser)
                                 (mconcat [briefDesc
                                          ,progDesc "Do stuff with entries"
                                          ,tnHeader
                                          ,tnFooter])))])

journalArgParser :: Parser Args
journalArgParser =
  (alt [subparser (command "list"
                           (info (helper <*>
                                  (pure (Journal List)))
                                 (mconcat [briefDesc
                                          ,progDesc "List the available journals"
                                          ,tnHeader
                                          ,tnFooter])))
       ,subparser (command "pp"
                           (info (helper <*>
                                  (Journal <$>
                                   (PrettyPrint <$>
                                    strArgument
                                      (mconcat [help "The journal to print."
                                               ,metavar "JOURNAL"
                                               ,value "default"
                                               ,showDefault]))))
                                 (mconcat [briefDesc
                                          ,progDesc "Pretty-print a journal."
                                          ,tnHeader
                                          ,tnFooter])))])
                                          
entryArgParser :: Parser Args
entryArgParser =
  (DoEntry <$>
   (strOption (mconcat [short 'j'
                       ,long "journal"
                       ,help (unwords ["Journal on which to operate."
                                      ,"See `tn journal list` for a list."])
                       ,metavar "NAME"
                       ,value "default"])) <*>
   (alt [subparser (command "add"
                            (info (helper <*>
                                   (pure Add))
                                  (mconcat [briefDesc
                                           ,progDesc "Add an entry."
                                           ,tnHeader
                                           ,tnFooter])))
        ,subparser (command "edit"
                            (info (helper <*> editEntryArgParser)
                                  (mconcat [briefDesc
                                           ,progDesc "Edit a journal entry."
                                           ,tnHeader
                                           ,tnFooter])))]))

editEntryArgParser :: Parser EntryAction
editEntryArgParser =
  fmap Edit (strArgument (mconcat [help "The entry to edit",metavar "ENTRY"]))

tnHeader :: InfoMod a
tnHeader =
  header (unwords ["Copyright (c) 2014-2015, Peter Harpending."
                  ,"Licensed under the FreeBSD license."])

tnFooter :: InfoMod a
tnFooter = mempty

runArgs :: Args -> IO ()
runArgs action =
  case action of
    DoEntry journalName entryAction ->
      let journalNameText = T.pack journalName
      in case entryAction of
           Add -> addEntry journalNameText
           Edit _ -> fail "FIXME: Not implemented"
    Journal journalAction ->
      case journalAction of
        PrettyPrint journalName ->
          ppJournal (T.pack journalName)
        List -> listJournals

mainWith :: [String] -> Maybe Args
mainWith =
  getParseResult .
  execParserPure parserPrefs argsParserInfo

parserPrefs :: ParserPrefs
parserPrefs =
  prefs (mconcat [disambiguate,showHelpOnError])

main :: IO ()
main =
  customExecParser parserPrefs argsParserInfo >>=
  runArgs
 
