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
-- Module      : Tn.Meat
-- Description : The interesting part of tn
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is the slightly less boring part of @tn@. This module contains
-- the code to edit diary entries.

module Tn.Meat where

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Char
import           Data.Data
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Data.Time
import           Data.Typeable
import           Data.Version
import           Paths_tn (version)
import           Safe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process
import           Tn.Potatoes
import           Tn.Static

-- |=== The meat 
--  
--  Okay, here's a function to edit the entry of a specific 'Day' (which
--  is from "Data.Time").
--  
--  Right now, it just launches the editor
editEntry :: Journal -> Day -> IO ()
editEntry j dy = do
  -- Look up the entry
  let ety = case Map.lookup dy j of
        -- If it's there, just get it
        Just e -> e
        -- Else, make a blank one
        Nothing -> ""
  print =<< popEditor ety dy

editToday :: Journal -> IO ()
editToday j = editEntry =<< today

-- |Edit the entry
popEditor :: Entry -> Day -> IO Entry
popEditor ety dy = do
  -- Open a temporary file
  (fp, h) <- openTempFile td thisApp
  -- No need for hFlush
  hSetBuffering h NoBuffering
  -- Write the entry to the file
  Tio.hPutStr h ety
  -- Write the daily template
  hPutStr h =<< dailyTemplate dy
  -- This is the user's editor
  e <- editor
  -- Open the editor, wait for it to close
  pc <- runCommand $ e <> " " <> fp
  _ <- waitForProcess pc
  hClose h
  -- return the file, comments filtered out
  fmap deleteTrailingWhitespace . fmap filterComments $ readFile fp 
