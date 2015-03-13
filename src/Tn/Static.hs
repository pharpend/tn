-- Tn - a simple journal program
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

-- | 
-- Module      : Tn.Static
-- Description : Boring static variables for tn
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This module just contains boring static stuff that is needed
-- throughout @tn@, like types, variables, instances, stuff like that.

module Tn.Static where

import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import           Data.Time
import           Data.Version
import           Paths_tn (version)
import           System.Directory
import           System.Environment
import           System.IO.Error

-- |== Less boring part
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
