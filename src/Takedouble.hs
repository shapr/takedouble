module Takedouble where

import Control.Monad (forM)
import Crypto.Hash
import Data.ByteString as BS (readFile)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath.Posix ((</>))
import System.IO (IOMode (ReadMode), hFileSize, withFile)
import Text.Printf (printf)

type HashFile =
  ( Digest SHA1, -- SHA1 cause maybe it's fast?
    FilePath -- filepath
  )

type FileSize = Integer

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

doTakedouble :: String
doTakedouble = "Takedouble"
