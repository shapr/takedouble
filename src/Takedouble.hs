module Takedouble where

import Control.Monad
import Control.Parallel.Strategies
import Crypto.Hash.SHA1
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Conc
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath.Posix ((</>))
import System.IO (IOMode (ReadMode), hFileSize, withFile)
import Text.Printf (printf)

type HashFile =
  ( Hash, -- SHA1 cause maybe it's fast?
    FilePath -- filepath
  )

type Hash = BS.ByteString

type FileSize = Integer

getHashes :: [(BL.ByteString, FilePath)] -> [HashFile]
getHashes bsfps =
  -- this pure isn't really needed, or the IO in the type signature
  (first hashlazy <$> bsfps) `using` parBuffer numCapabilities rdeepseq

getConts :: FileSize -> [FilePath] -> IO [(BL.ByteString, FilePath)]
getConts fsize fps = do
  notTooBig <- filterM (checkSize fsize) fps
  conts <- traverse BL.readFile notTooBig
  pure $ zip conts fps

checkSize :: Integer -> FilePath -> IO Bool
checkSize allowed h = do
  size <- withFile h ReadMode hFileSize
  pure $ size <= allowed

-- get all the FilePath values
getFileNames :: FilePath -> IO [FilePath]
getFileNames curDir = do
  names <- getDirectoryContents curDir
  let dirs = filter (`notElem` [".", ".."]) names
  files <- forM dirs $ \path -> do
    let path' = curDir </> path
    exists <- doesDirectoryExist path'
    if exists
      then getFileNames path'
      else pure $ pure path'
  return $ concat files

getRecursiveContents :: FilePath -> FileSize -> IO [HashFile]
getRecursiveContents curDir maxsize = do
  names <- getDirectoryContents curDir
  let dirs = filter (`notElem` [".", ".."]) names
  files <- forM dirs $ \path -> do
    let path' = curDir </> path
    exists <- doesDirectoryExist path'
    if exists
      then getRecursiveContents path' maxsize
      else genFileHash path' maxsize
  return $ concat files

genFileHash :: FilePath -> FileSize -> IO [HashFile]
genFileHash path maxsize = do
  size <- withFile path ReadMode hFileSize
  if size <= maxsize
    then BS.readFile path >>= \bs -> return [(hash bs, path)]
    else return []

findDuplicates :: FilePath -> FileSize -> IO ()
findDuplicates dir bytes = do
  exists <- doesDirectoryExist dir
  if exists
    then getRecursiveContents dir bytes >>= findSameHashes
    else printf "Sorry, the directory \"%s\" does not exist...\n" dir

findDuplicates' :: FilePath -> FileSize -> IO ()
findDuplicates' dir bytes = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      getFileNames dir >>= getConts bytes >>= pure . getHashes >>= findSameHashes
    else printf "Sorry, the directory \"%s\" does not exist...\n" dir

findSameHashes :: [HashFile] -> IO ()
findSameHashes [] = return ()
findSameHashes ((hash, fp) : xs) = do
  case lookup hash xs of
    (Just dupFile) ->
      printf
        "===========================\n\
        \Found duplicate:\n\
        \=> %s \n\
        \=> %s \n\n"
        fp
        dupFile
        >> findSameHashes xs
    _ -> findSameHashes xs
