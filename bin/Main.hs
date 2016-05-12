module Main where

import           Data.Aeson hiding (encode)
import qualified Data.ByteString as B
import           Data.FileEmbed
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Yaml
import           GHC.Generics
import           Options.Applicative.Simple
import qualified Paths_tn as P
import           System.Directory
import           System.IO
import           System.Pager

main :: IO ()
main =
  do (_, cmd) <-
       simpleOptions $(simpleVersion P.version)
                     appName
                     "Simple journaling program"
                     (pure ())
                     parser
     runCommand cmd
  where
    appName = "tn"
    parser = do licenseCmd
                newEntryCommand
    licenseCmd =
      addCommand "license"
                 "Show the license (ISC)"
                 ShowLicense
                 (switch (mconcat [ long "no-pager"
                                  , help "Do not pipe to the system pager"
                                  , showDefault
                                  ]))
    newEntryCommand =
      addCommand "new"
                 "Add a new entry"
                 id
                 (NewEntry <$> altConcat [ fmap Argument
                                                (strArgument (mconcat [ help "Entry text"
                                                                      , metavar "TEXT"
                                                                      ]))
                                         , fmap FromFile
                                                (strOption (mconcat [ help "Input file"
                                                                    , metavar "PATH"
                                                                    , long "input"
                                                                    , short 'i'
                                                                    ]))
                                         , flag' Stdin
                                                 (mconcat [ help "Read entry text from stdin"
                                                          , long "stdin"
                                                          ])
                                         ]
                           <*> altConcat [ flag' Stdout
                                                 (mconcat [ help "Print to stdout instead of a file"
                                                          , long "stdout"
                                                          ])
                                         , fmap ToFile
                                                (strOption (mconcat [ help "Output file"
                                                                    , metavar "PATH"
                                                                    , long "output"
                                                                    , short 'o'
                                                                    ]))
                                         , pure DefaultOutput
                                         ])
    altConcat = foldr (<|>) empty
    runCommand =
      \case
        ShowLicense noPager
          | noPager -> T.putStrLn licenseText
          | otherwise -> printOrPage licenseText
        NewEntry i o ->
          do inputText <-
               case i of
                 Argument s -> return (T.pack s)
                 FromFile fp ->
                   do absPath <- makeAbsolute fp
                      T.readFile absPath
                 Stdin -> T.hGetContents stdin
             cjp <- defaultPath
             currentJournal <-
               decodeFileEither cjp
               >>= \case
                     Left e -> fail (show e)
                     Right x -> return x
             currentTime <- getCurrentTime
             let newJournal = V.snoc currentJournal
                                     (Entry currentTime inputText)
                 newJournalYml = encode newJournal
             case o of
               Stdout -> B.hPut stdout newJournalYml
               ToFile f ->
                 do absFile <- makeAbsolute f
                    B.writeFile absFile newJournalYml
               DefaultOutput ->
                 B.writeFile cjp newJournalYml
    licenseText = T.decodeUtf8 $(embedFile "LICENSE")
    dataPath = getXdgDirectory XdgData appName
    defaultPath =
      do dp <- dataPath
         createDirectoryIfMissing True dp
         let pth = mappend dp "/journal.yaml"
         fexists <- doesFileExist pth
         if fexists
           then return pth
           else do T.writeFile pth "[]"
                   return pth

data Command = NewEntry Input Output
             | ShowLicense Bool
  deriving (Eq, Show)

data Input = Argument String
           | FromFile FilePath
           | Stdin
  deriving (Eq, Show)

data Output = ToFile FilePath
            | Stdout
            | DefaultOutput
  deriving (Eq, Show)

type Journal = Vector Entry

data Entry = Entry { entryTime :: UTCTime
                   , entryText :: Text
                   }
  deriving (Eq, Show, Generic)

instance ToJSON Entry
instance FromJSON Entry
