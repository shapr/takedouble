module Takedouble (findDuplicates, getFileNames) where

import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import Data.List (group)
import Data.Traversable (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath.Posix ((</>))
import System.IO
  ( Handle,
    IOMode (ReadMode),
    SeekMode (SeekFromEnd),
    hFileSize,
    hSeek,
    withFile,
  )

data File = File
  { filepath :: FilePath,
    filesize :: Integer,
    firstchunk :: Hash, -- will the chunks be lazy?
    lastchunk :: Hash
    -- allchunks :: Hash
  }
  deriving (Ord, Show)

-- don't compare by filepath!
instance Eq File where
  (File _ fs1 firstchunk1 lastchunk1) == (File _ fs2 firstchunk2 lastchunk2) =
    (fs1, firstchunk1, lastchunk1) == (fs2, firstchunk2, lastchunk2)

findDuplicates :: [FilePath] -> IO [[File]]
findDuplicates filenames = do
  files <- mapM loadFile filenames
  pure $ filter (\x -> 1 < length x) $ group files

loadFile :: FilePath -> IO File
loadFile fp = do
  (fsize, firstchunk, lastchunk) <- withFile fp ReadMode getChunks
  pure $ File fp fsize firstchunk lastchunk

getChunks :: Handle -> IO (Integer, Hash, Hash)
getChunks h = do
  fsize <- hFileSize h
  begin <- BS.hGet h 2048
  hSeek h SeekFromEnd 2048
  end <- BS.hGet h 2048
  pure (fsize, hash begin, hash end)

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
  pure $ concat files

type Hash = BS.ByteString
