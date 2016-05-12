module Main where

import Options.Applicative
import System.IO
import Tn

data Command = NewEntry Input Output
             | ShowLicense Bool
             | ShowVersion
  deriving (Eq, Show)

data Input = Argument String
           | FromFile FilePath
           | Stdin
  deriving (Eq, Show)

data Output = ToFile FilePath
            | Stdout
            | DefaultOutput
  deriving (Eq, Show)

main :: IO ()
main =
  do result <- customExecParser tnPrefs (infoHelper tnParser tnInfo)
     hSetBuffering stdin NoBuffering
     case result of
        ShowLicense noPager -> printLicense noPager
        ShowVersion -> putStrLn version
        NewEntry i o ->
          do input <- case i of
                        Argument s -> return s
                        FromFile fp -> readFile fp
                        Stdin -> getContents
             currentJournal <- readJournal
             newJournal <- addEntry currentJournal <$> mkEntry input
             case o of
               Stdout -> printJournal newJournal False
               ToFile f -> writeJournalFile newJournal f
               DefaultOutput -> writeJournal newJournal

tnPrefs :: ParserPrefs
tnPrefs =
  prefs $ mconcat [ disambiguate
                  , showHelpOnError
                  ]

infoHelper :: Parser a -> InfoMod a -> ParserInfo a
infoHelper a = info (helper <*> a)

tnInfo :: InfoMod Command
tnInfo = mconcat [ briefDesc
                 , progDesc "Simple journal-keeping program"
                 ]

tnParser :: Parser Command
tnParser =
  altConcat $ fmap subparser [ licenseCmd
                             , versionCmd
                             , newEntryCmd
                             ]


licenseCmd :: Mod CommandFields Command
licenseCmd = 
  command "license" $
    infoHelper licenseParser
               (mappend briefDesc
                        (progDesc "Show the license (ISC)"))
  where
    licenseParser = 
      ShowLicense <$> switch (mconcat [ long "no-pager"
                                      , help "Do not pipe to the system pager"
                                      , showDefault
                                      ])

versionCmd :: Mod CommandFields Command
versionCmd = 
  command "version" $
    infoHelper (pure ShowVersion)
               (mappend briefDesc
                        (progDesc "Show the version"))

  
newEntryCmd :: Mod CommandFields Command
newEntryCmd =
  command "new"
          (infoHelper (NewEntry <$> inputOpts <*> outputOpts)
                      (mappend briefDesc
                               (progDesc "New entry")))
  where
    inputOpts = 
      altConcat [ FromFile <$> strOption (mconcat [ help "Input file"
                                                  , metavar "PATH"
                                                  , long "input"
                                                  , short 'i'
                                                  ])
                , flag' Stdin (mconcat [ help "Read entry text from stdin"
                                       , long "stdin"
                                       ])
                , Argument <$> strArgument (mconcat [help "Entry text", metavar "TEXT"])
                ]
    outputOpts = altConcat [ flag' Stdout (mconcat [ help "Print to stdout instead of a file"
                                                   , long "stdout"
                                                   ])
                           , ToFile <$> strOption (mconcat [ help "Output file"
                                                           , metavar "PATH"
                                                           , long "output"
                                                           , short 'o'
                                                           ])
                           , pure DefaultOutput
                           ]

