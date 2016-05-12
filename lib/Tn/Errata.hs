-- |The Tn license, version, that fun stuff
module Tn.Errata where

import           Control.Applicative
import           Data.FileEmbed
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Options.Applicative.Simple (simpleVersion)
import qualified Paths_tn as P
import           System.Pager (printOrPage)

-- |Text of the ISC license
licenseText :: Text
licenseText = T.decodeUtf8 $(embedFile "LICENSE")

-- |Print the license
printLicense :: Bool            -- ^Whether or not to enable use of a pager.
             -> IO ()
printLicense p
  | p = printOrPage licenseText
  | otherwise = T.putStr licenseText

-- |Tn's version, as a 'String'
version :: String
version = $(simpleVersion P.version)

-- |Name of the application
appName :: String
appName = "tn"

-- |Concatenate a number of 'Alternate' values.
-- 
-- > altConcat = foldr (<|>) empty
altConcat :: (Alternative f, Foldable t) => t (f a) -> f a
altConcat = foldr (<|>) empty
