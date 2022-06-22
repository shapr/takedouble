{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Hedgehog
import Hedgehog.Main
-- import System.Directory
import System.FilePath.Posix ((</>))
import System.IO.Temp
import Takedouble

main :: IO ()
main = defaultMain [checkParallel $$(discover)]

prop_FileEq_no_check_filename :: Property
prop_FileEq_no_check_filename = property $ do
  File "a" 1024 "abc" "def" === File "b" 1024 "abc" "def"

-- prop_Find_Duplicates :: Property
-- prop_Find_Duplicates = property $
--   do
--     withTempDirectory "/tmp" "foo" $ do
--       \dir -> do
--         _ <- genTempFiles dir
--         files <- findPossibleDuplicates [dir]
--         if 1 < length files then pure () else error "no"

-- dir <- liftIO $ genTempFiles
-- files <- liftIO $ findPossibleDuplicates [dir]
-- True === (1 < length files)

genTempFiles :: FilePath -> IO FilePath
genTempFiles dir = do
  -- dir <- getTemporaryDirectory
  writeFile (dir </> "dup1") "duplicate file"
  writeFile (dir </> "dup2") "duplicate file"
  writeFile (dir </> "uniq1") "uniq file"
  pure dir
