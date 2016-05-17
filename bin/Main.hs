module Main where

import qualified Data.ByteString as B
import           Data.FileEmbed
import           Options.Applicative
import           Options.Applicative.Helper
import           System.IO
import           Tn

main :: IO ()
main =
  do result <- helperExecParser tnParser
                                (fpDesc "Simple journal-keeping program")
     hSetBuffering stdin NoBuffering
     case result of
       ShowLicense -> B.hPut stdout $(embedFile "LICENSE")
       ShowVersion -> putStrLn version
       NewEntry i ->
         do currentJournal <- readJournal
            newJournal <- addEntry currentJournal <$> mkEntry i
            writeJournal newJournal
       Cat ->
         do currentJournal <- readJournal
            printJournal currentJournal False
  where tnParser = altconcat [version', license, cat, newEntry]
        version' = flag' ShowVersion
                         (mappend (help "Print the version")
                                  (long "version"))
        license = flag' ShowLicense
                        (mappend (help "Print the license")
                                 (long "license"))
        cat = flag' Cat
                    (mconcat [ help "Print the current journal"
                             , long "cat"
                             , short 'c'
                             ])
        catDesc = fpDesc "Print the current journal"
        newEntry = NewEntry <$> strArgument (help "Entry text")

data Command = NewEntry String
             | Cat
             | ShowLicense
             | ShowVersion
  deriving (Eq, Show)
