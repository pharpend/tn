-- |Tn - a simple journal program
-- Copyright (C) 2015 Peter Harpending
-- 
-- === License disclaimer 
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
-- 
-- == Boring code 
-- 
-- Here's the module declaration:

module Main where

-- |It's not very interesting. Here are some imports:

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Data
import qualified Data.Map.Lazy as Map
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time
import           Data.Typeable
import           Data.Version
import           Data.Yaml
import           Paths_tn (version)
import           Safe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process

-- |Again, not very interesting.
-- 
-- == Less boring part
-- 
-- Now, here's the slightly less boring part.
-- 
-- === 'Journal' type
-- 
-- We need a type for the journal. This is Haskell, we need a type for
-- everything. I could have the 'Journal' type be some crazy type I make,
-- but this seems easier, and simpler, at least for getting everything
-- started:
type Journal = Map.Map Day Entry
-- |Entry is just text
type Entry = T.Text

-- |=== Variables
-- 
-- Alright, great. Here are some variables that we're going to need to
-- just use whenever.
-- 
-- First of all, we have the name of the application. It's @tn@ right
-- now, but I might want to change it in the future, so I'm factoring
-- it out into a variable.
thisApp :: String
thisApp = "tn"

-- |Next is the /application user data directory/ - this is the
-- directory in which user data specific to this application is
-- stored.
tnDir :: IO FilePath
tnDir = getAppUserDataDirectory thisApp

-- |Next, the editor. Eventually, I'll want to factor this out into a
-- configuration option in a config file or something.
editor :: IO String
editor = tryIOError (getEnv "EDITOR") >>= \case
           Left _  -> return "nano"
           Right e -> return e

-- |Temporary directory
td :: FilePath
td = "/tmp"

-- |Dummy help thing
help :: IO ()
help = putStrLn "No help for you"

-- | Will output @tn v.4.12.4@, if @4.12.4@ is the version.
tnVersion :: String
tnVersion = showVersion version

-- |==== Technically not variables
-- 
-- Okay, these aren't really variables, but they sort of serve the same purpose
today :: IO Day
today = utctDay <$> getCurrentTime

getHypotheticalDataFileName :: String -> IO FilePath
getHypotheticalDataFileName s = do
  dir <- tnDir
  return $ dir <> s

-- |Initialize directory
initialize :: IO ()
initialize = createDirectoryIfMissing False =<< tnDir

-- |Subtract some number of days from today. So, yesterday would be
-- @todayMinus 1@.
todayMinus :: Integer -> IO Day
todayMinus i = addDays (-1 * i) <$> today

-- |=== Let's get 'main' out of the way
-- 
-- I don't like putting 'main' at the end, so I'm just going to put it
-- here. We'll define each of the function later
-- 
main :: IO ()
main = do
  -- Get the arguments
  args <- getArgs
  -- @--help@ gets first priority
  if or ["--help" `elem` args, "-h" `elem` args]
    then help
    else if "--version" `elem` args
           then putStrLn tnVersion
           else runTn

-- |Main was getting a bit long, so I took the latter half of it and
-- put it into another function.
runTn :: IO ()
runTn = do
  -- First, initialize!
  initialize
  -- First, let's get the command line arguments
  args <- getArgs
  -- Then split'm up
  let fstarg = headMay args
      rstof = tailMay args
  -- Case statement them
  case fstarg of
    Nothing -> editToday
    Just cmd ->
      case cmd of
        "edit" ->
          case rstof of
            Just (s:_) ->
              case readMay s of
                Nothing -> help
                Just d  -> editEntry d
            _ -> help
        _ -> help

-- |=== The meat & potatoes
--  
--  Okay, here's a function to edit the entry of a specific 'Day' (which
--  is from "Data.Time").
--  
--  Right now, it's just an alias to launch the editor

editEntry :: Day -> IO ()
editEntry = launchEditor

-- |Launch 'editor' to edit the entry for today.
launchEditor :: Day -> IO ()
launchEditor dy = do
  -- The filepath and the handl
  (fp, h) <- openTempFile td thisApp
  e <- editor
  -- The process
  pc <- runCommand $ e <> " " <> fp
  ec <- waitForProcess pc
  print ec
  hClose h
  print =<< readFile fp

editToday :: IO ()
editToday = editEntry =<< today

