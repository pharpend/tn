-- tn - journaling program
-- Copyright (c) 2014-2016, Peter Harpending.
-- 
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Utility.Tn.Types
-- Description : The Types for Tn
-- Copyright   : Copyright (c) 2014-2016, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : GHC

module Utility.Tn.Types where

import Control.Monad (mzero)
import Data.Aeson
import Data.Ord (comparing)
import Data.Text
import Data.Time
import Data.Vector

-- |The core data type
type Tn = Vector Journal

-- |A journal
data Journal =
  Journal {journalTitle :: Text
          ,journalEntries :: Vector Entry}
  deriving (Show,Eq)

-- |A journal Entry
data Entry =
  Entry {entryTime :: UTCTime
        ,entryEditTimes :: Vector UTCTime
        ,entryText :: Text}
  deriving (Show,Eq)

-- |Compares on the value of 'journalTitle'
instance Ord Journal where
  compare = comparing journalTitle

instance FromJSON Journal where
  parseJSON (Object v) =
    Journal <$> v .: _journal_title <*> v .: _journal_entries
  parseJSON _ = mzero

instance ToJSON Journal where
  toJSON (Journal ttl etys) =
    object [_journal_title .= ttl,_journal_entries .= etys]

-- |Compares on 'entryTime'
instance Ord Entry where
  compare = comparing entryTime
  
instance FromJSON Entry where
  parseJSON (Object v) =
    Entry <$> v .: _entry_time <*> v .: _entry_edits <*> v .: _entry_text
  parseJSON _ = mzero
  
instance ToJSON Entry where
  toJSON (Entry time edits txt) =
    object [_entry_time .= time,_entry_edits .= edits,_entry_text .= txt]

-- ** Aeson label names

_entry_time :: Text
_entry_time = "entry-time"

_entry_edits :: Text
_entry_edits = "entry-edits"

_entry_text :: Text
_entry_text = "entry-text"

_journal_title :: Text
_journal_title = "journal-title"

_journal_entries :: Text
_journal_entries = "journal-entries"
