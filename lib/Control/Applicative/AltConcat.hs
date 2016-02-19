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
-- Module      : Control.Applicative.AltConcat
-- Description : Extra stuff for control.Applicative
-- Copyright   : Copyright (c) 2014-2016, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : GHC
-- 
-- This module re-exports "Control.Applicative"

module Control.Applicative.AltConcat
       ( module Control.Applicative
       , altConcat
       )
       where

import Control.Applicative (Alternative(..))
import qualified Control.Applicative

-- |Equivalent of 'mconcat' for 'Alternative's
altConcat :: (Alternative f, Foldable t) => t (f a) -> f a
altConcat = foldl (<|>) empty
