{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Hedgehog
import Kudzu
import System.FilePath.Posix ((</>))
import System.IO.Temp
import Takedouble

main :: IO ()
main = do
  res <- testUntilSameHHMany 3 [prop_FileEq_no_check_filename, prop_Find_Duplicates, prop_Glob]
  print res

prop_FileEq_no_check_filename :: Property
prop_FileEq_no_check_filename = property $ do
  File "a" 1024 "abc" "def" === File "b" 1024 "abc" "def"

prop_Find_Duplicates :: Property
prop_Find_Duplicates = property $
  do
    files <- liftIO $
      withTempDirectory "/tmp" "foo" $
        \dir -> do
          _ <- genTempFiles dir
          filenames <- getFileNames dir
          findPossibleDuplicates filenames Nothing
    all allTheSame files === True

prop_Glob :: Property
prop_Glob = property $
  do
    files <- liftIO $
      withTempDirectory "/tmp" "foo" $
        \dir -> do
          _ <- genTempFiles dir
          filenames <- getFileNames dir
          findPossibleDuplicates filenames $ Just "**"
    length files === 0

genTempFiles :: FilePath -> IO FilePath
genTempFiles dir = do
  writeFile (dir </> "dup1") "duplicate file"
  writeFile (dir </> "dup2") "duplicate file"
  writeFile (dir </> "uniq1") "uniq file"
  pure dir

allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = False -- For the duplicate finder we want this case to fail. Semantically this makes no sense.
allTheSame xs = all (== head xs) (tail xs)
