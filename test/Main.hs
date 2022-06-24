{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Hedgehog
import Hedgehog.Main
import System.FilePath.Posix ((</>))
import System.IO.Temp
import Takedouble

main :: IO ()
main = defaultMain [checkParallel $$(discover)]

prop_FileEq_no_check_filename :: Property
prop_FileEq_no_check_filename = property $ do
  File "a" 1024 "abc" "def" === File "b" 1024 "abc" "def"

prop_Find_Duplicates :: Property
prop_Find_Duplicates = property $
   do
     files <- liftIO $ withTempDirectory "/tmp" "foo" $
       \dir -> do
         _ <- genTempFiles dir
         filenames <- getFileNames dir
         findPossibleDuplicates filenames
     (all allTheSame files) === True

prop_Glob :: Property
prop_Glob = property $
   do
     files <- liftIO $ withTempDirectory "/tmp" "foo" $
       \dir -> do
         _ <- genTempFiles dir
         filenames <- getFileNames dir
         findPossibleDuplicates filenames $ Just "**"
     (length files) === 0
    

genTempFiles :: FilePath -> IO FilePath
genTempFiles dir = do
  -- dir <- getTemporaryDirectory
  writeFile (dir </> "dup1") "duplicate file"
  writeFile (dir </> "dup2") "duplicate file"
  writeFile (dir </> "uniq1") "uniq file"
  pure dir

allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = False -- For the duplicate finder we want this case to fail. Semantically this makes no sense.
allTheSame xs = and $ map (== head xs) (tail xs)
