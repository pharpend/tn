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

-- |Here are some imports:

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Char
import           Data.Data
import qualified Data.Map.Lazy as Map
import           Data.Monoid
import qualified Data.Text as T
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
import           Tn.Meat
import           Tn.Potatoes
import           Tn.Static

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
  initialize
  args <- getArgs
  let fstarg = headMay args
      rstof = tailMay args
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

