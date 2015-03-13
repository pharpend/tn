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
-- Module      : Tn.Potatoes
-- Description : The slightly more interesting stuff in tn
-- Copyright   : Copyright (C) 2015 Peter Harpending
-- License     : GPL-3
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : UNIX/GHC
-- 
-- This is the slightly more interesting stuff in @tn@. The name is a
-- pun on the idiom \"meat & potatoes\"; this is the potatoes,
-- "Tn.Meat" is the meat. The meat is obviously better than the
-- potatoes.

module Tn.Potatoes where

import           Control.Applicative
import           Data.Char
import           Data.Monoid
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           System.Directory
import           Tn.Static

-- |==== Technically not variables
-- 
-- Okay, these aren't really variables, but they sort of serve the
-- same purpose
-- 
-- |This is the text that will show up in the editor
dailyTemplate :: Day -> IO String
dailyTemplate dy = do
  return $ mconcat
             [ "\n"
             , "\n;; Date: "
             , show dy
             , "\n;;"
             , "\n;; By the way, that date is in UTC time, so it might be different than the date"
             , "\n;; in your local time."
             , "\n;; "
             , "\n;; Lines starting with ';;' will be ignored."
             , "\n"
             ]

-- |Given some file name, get the canonical path
getHypotheticalDataFileName :: String -> IO FilePath
getHypotheticalDataFileName s = do
  dir <- tnDir
  return $ dir <> s

-- |Initialize the data directory
initialize :: IO ()
initialize = createDirectoryIfMissing False =<< tnDir

-- |Today
today :: IO Day
today = utctDay <$> getCurrentTime

-- |Subtract some number of days from 'today'. So, yesterday would be
-- @todayMinus 1@.
todayMinus :: Integer -> IO Day
todayMinus i = addDays (-1 * i) <$> today

-- |Get rid of the comments
filterComments :: Text -> Text
filterComments = T.unlines . filter notAComment . T.lines
  where 
    notAComment :: Text -> Bool
    notAComment s = T.take 2 (noLeadingWhitespace s) /= ";;"
    noLeadingWhitespace :: Text -> Text
    noLeadingWhitespace = T.dropWhile isSpace

-- |Delete trailing whitespace, but make sure it ends with a newline
deleteTrailingWhitespace :: Text -> Text
deleteTrailingWhitespace = T.unlines . composeChain . T.lines
  where
    composeChain :: [Text] -> [Text]
    composeChain = addNewlines . getRidOfTrailingBlanks . map deleteAllTrails 
    deleteAllTrails :: Text -> Text
    deleteAllTrails = T.dropWhileEnd isSpace
    getRidOfTrailingBlanks :: [Text] -> [Text]
    getRidOfTrailingBlanks = dropWhileEnd T.null
    addNewlines :: [Text] -> [Text]
    addNewlines = map (<> "\n")
