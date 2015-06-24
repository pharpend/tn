{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 
-- Module      : Tn
-- Description : Main module for the tn library
-- Copyright   : Copyright (c) 2014-2015, Peter Harpending.
-- License     : BSD-2
-- Maintainer  : Peter Harpending <peter@harpending.org>
-- Stability   : experimental
-- Portability : POSIX
-- 

module Tn 
  ( -- |Re-exported for convenience
    module Control.Exceptional
  , fromString
    -- * Core data types
  , Journal(..)
  , Entry(..)
  , Title
  , titleAcceptChars
  , mkTitle
  , unTitle
  , Synopsis
  , mkSynopsis
  , unSynopsis
  )
  where

#if !(MIN_VERSION_base (4,8,0))
import Data.Monoid
#endif

import Control.Exceptional
import Control.Monad (mzero)
import Data.Ord (comparing)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Yaml

-- |A journal
data Journal =
  Journal {journalTitle :: Title
          ,journalEntries :: Vector Entry}
  deriving (Show,Eq)

-- |Compares on the value of 'journalTitle'
instance Ord Journal where
  compare = comparing journalTitle

instance FromJSON Journal where
  parseJSON (Object v) = Journal <$> v .: "journal-title"
                                 <*> v .: "journal-entries"
  parseJSON _ = mzero

instance ToJSON Journal where
  toJSON (Journal ttl etys) = object [ "journal-title" .= ttl
                                     , "journal-entries" .= etys
                                     ]

-- |A journal Entry
data Entry =
  Entry {entryTime :: UTCTime
        ,entrySynopsis :: Synopsis
        ,entryText :: Maybe Text}
  deriving (Show,Eq)

-- |Compares on 'entryTime'
instance Ord Entry where
  compare = comparing entryTime
  
instance FromJSON Entry where
  parseJSON (Object v) = Entry <$> v .: "entry-time"
                               <*> v .: "entry-synopsis"
                               <*> v .:? "entry-text"
  parseJSON _ = mzero
  
instance ToJSON Entry where
  toJSON (Entry time syn (Just txt)) = object [ "entry-time" .= time
                                              , "entry-synopsis" .= syn
                                              , "entry-text" .= txt
                                              ]
  toJSON (Entry time syn Nothing) = object [ "entry-time" .= time
                                           , "entry-text" .= syn
                                           ]

-- |The title of a journal. This is a newtype over 'Text'. 0 < number of chars < 33, only
-- 'titleAcceptChars' allowed.
-- 
-- Use 'mkTitle' to make a title, or use the 'IsString' instance.
newtype Title = Title { unTitle :: Text }
  deriving (Eq, Show)

-- |A 'Vector' of acceptable characters for the title
-- 
-- > titleAcceptChars = V.fromList $ mconcat [ ['A' .. 'Z']
-- >                                         , ['a' .. 'z']
-- >                                         , ['0' .. '9']
-- >                                         , "-_"
-- >                                         ]
titleAcceptChars :: Vector Char
titleAcceptChars = V.fromList $ mconcat [ ['A' .. 'Z']
                                        , ['a' .. 'z']
                                        , ['0' .. '9']
                                        , "-_"
                                        ]

-- |Make a 'Title', returning a 'Failure' on invalid input
mkTitle :: Text -> Exceptional Title
mkTitle ttl
  | T.null ttl = Failure "Title must not be empty"
  | 33 <= T.length ttl = Failure "Title may be at most 32 characters"
  | not $ T.all (`V.elem` titleAcceptChars) ttl = 
      Failure $ mappend "Title may only contain the following characters: "
                        (V.toList titleAcceptChars)
  | otherwise = Success $ Title ttl

instance Ord Title where
  compare = comparing unTitle

-- |Note that this will throw an error on invalid input
instance IsString Title where
  fromString s = case mkTitle (T.pack s) of
                   Success x -> x
                   Failure msg -> error msg

instance FromJSON Title where
  parseJSON (String s) = runExceptional $ mkTitle s
  parseJSON _ = mzero

instance ToJSON Title where
  toJSON (Title s) = String s

-- |A synopsis is any bunch of text that doesn't have newlines.
newtype Synopsis = Synopsis {unSynopsis :: Text}
  deriving (Eq, Show)

-- |Try to make a 'Synopsis', returning 'Failure' if there's a newline
-- or a carriage return.
mkSynopsis :: Text -> Exceptional Synopsis
mkSynopsis txt
  | T.any (`elem` ("\r\n" :: String)) txt = 
      fail "Synopsis cannot have newlines (\\n) or carriage returns (\\r)"
  | otherwise = return $ Synopsis txt

instance Ord Synopsis where
  compare = comparing unSynopsis

-- |Note that this will throw an error on invalid input
instance IsString Synopsis where
  fromString s = case mkSynopsis (T.pack s) of
                   Success x -> x
                   Failure msg -> error msg

instance FromJSON Synopsis where
  parseJSON (String s) = runExceptional $ mkSynopsis s
  parseJSON _ = mzero

instance ToJSON Synopsis where
  toJSON (Synopsis s) = String s
