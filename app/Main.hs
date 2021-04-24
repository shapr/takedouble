module Main where

import Data.List (sort)
import System.Environment (getArgs, getProgName)
import Takedouble (File (..), checkFullDuplicates, findDuplicates, getFileNames)
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
        likelyDups <- findDuplicates filenames -- each element in the list is a list of files that are likely duplicates
        let fnamesOnly = (filepath <$>) `fmap` likelyDups -- convert to a nested list of FilePath
        dups <- mapM checkFullDuplicates fnamesOnly
        mapM_ (\x -> putStr "\n" >> mapM_ putStrLn x) (sort $ concat dups)
    _ -> do
      name <- getProgName
      printf "Something went wrong - please use ./%s <dir>\n" name
