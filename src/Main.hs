module Main where

import           Control.Applicative
import qualified Data.ByteString as B
import           Data.Data
import qualified Data.Map.Lazy as Map
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Time
import           Data.Typeable
import           Data.Yaml
import           Paths_tn (version)
import           Safe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.IO.Temp
import           System.Process

type Journal = Map.Map Day Entry
type Entry = T.Text

thisApp :: String
thisApp = "tn"

today :: IO Day
today = utctDay <$> getCurrentTime

todayMinus :: Integer -> IO Day
todayMinus i = addDays (-1 * i) <$> today

editEntry :: Day -> IO ()
editEntry dy = do
  let daystr = show dy
  withSystemTempFile thisApp launchEditor
  putStr "Your editor is: \n    "
  putStrLn =<< editor

launchEditor :: FilePath -> Handle -> IO ()
launchEditor fp h = do
  hPutStrLn h "HAHAHA"
  hPutStrLn stdout =<< hGetContents h
  -- myEditor <- editor
  -- ph <- runCommand $ myEditor <> " " <> fp
  -- waitForProcess ph >>= \case
  --   ExitSuccess -> 
      
  --   q -> hPutStrLn stderr "Editor failed" >> exitWith q

editToday :: IO ()
editToday = editEntry =<< today

help = putStrLn "yay"

tnVersion :: IO ()
tnVersion = print version

editor :: IO String
editor = tryIOError (getEnv "EDITOR") >>= \case
           Left _  -> return "nano"
           Right e -> return e

tnDir :: IO FilePath
tnDir = getAppUserDataDirectory thisApp

getHypotheticalDataFileName :: String -> IO FilePath
getHypotheticalDataFileName s = do
  dir <- tnDir
  return $ dir <> s

main :: IO ()
main = do
  args <- getArgs
  if or ["--help" `elem` args, "-h" `elem` args]
    then help
    else if "--version" `elem` args
      then tnVersion
      else runTn

runTn :: IO ()
runTn = do
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
