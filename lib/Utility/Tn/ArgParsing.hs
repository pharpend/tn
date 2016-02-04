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
-- Module      : Utility.Tn.ArgParsing
-- Description : Argument parsing innerds for tn
-- Copyright   : Copyright (c) 2014-2016, Peter Harpending.
-- License     : GPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : GHC

module Utility.Tn.ArgParsing where

import Options.Applicative

-- |Equivalent of 'mconcat' for 'Alternative's
altConcat :: Alternative f
            => [f a] -> f a
altConcat [] = empty
altConcat (x:xs) = x <|> altConcat xs

-- |What does the user want to do?
data TnAction
  = ListJournals
  | PrintLicense
  deriving (Show, Eq)

-- |Parses the 'TnAction'
actionParser :: Parser TnAction
actionParser = altConcat (fmap subparser [printLicenseCmd,listJournalsCmd])

-- |Command to print the license
printLicenseCmd :: Mod CommandFields TnAction
printLicenseCmd =
  command "license"
          (info empty
                (mconcat [fullDesc,progDesc "Print the license (GPL-3)."]))

-- |Command to list the journals
listJournalsCmd :: Mod CommandFields TnAction
listJournalsCmd = mempty
