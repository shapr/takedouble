module Main where

import System.Environment (getArgs, getProgName)
import Takedouble (findDuplicates, getFileNames)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] ->
      do
        print $ "reading " <> dir
        filenames <- getFileNames dir
        print $ "comparing " <> show (length filenames) <> " files"
        dups <- findDuplicates filenames
        print dups
    _ -> do
      name <- getProgName
      printf "Something went wrong - please use ./%s <dir> <bytes>\n" name
