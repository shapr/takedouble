module Main where

import Data.List (sort)
import System.Environment (getArgs)
import Takedouble
  ( File (filepath),
    checkFullDuplicates,
    findPossibleDuplicates,
    getFileNames,
  )

fetchResults :: [FilePath] -> Maybe String -> IO ()
fetchResults filenames glob = do
  putStrLn $ "comparing " <> show (length filenames) <> " files"
  likelyDups <- findPossibleDuplicates filenames glob -- each element in the list is a list of files that are likely duplicates
  let fnamesOnly = (filepath <$>) `fmap` likelyDups -- convert to a nested list of FilePath
  dups <- mapM checkFullDuplicates fnamesOnly
  mapM_ (\x -> putStr "\n" >> mapM_ putStrLn x) (sort $ concat dups)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["help"] -> do
      putStrLn "Usage: takedouble DIR [GLOB]"
      putStrLn "Description:"
      putStrLn "\tDIR  - starting search directory"
      putStrLn "\tGLOB - optional glob from filepattern library to exclude from results."
      putStrLn ""
      putStrLn "Examples:"
      putStrLn "\ttakedouble '~/code/' '**/.git/**' # This will exclude .git paths from results."
    [dir] ->
      do
        putStrLn $ "reading " <> dir
        filenames <- getFileNames dir
        fetchResults filenames Nothing
    [dir, glob] -> do
      putStrLn $ "reading " <> dir
      filenames <- getFileNames dir
      fetchResults filenames $ Just glob
    _ -> do
      putStrLn "Something went wrong - See 'takedouble help' for usage."
