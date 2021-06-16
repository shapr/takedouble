module Takedouble (findDuplicates, getFileNames, checkFullDuplicates, File (..)) where

import Control.Monad.Extra (filterM, ifM)
import qualified Data.ByteString as BS
import Data.List (group, sort, sortOn)
import Data.List.Extra (groupOn)
import Data.Traversable (forM)
import System.Directory (doesDirectoryExist, doesPathExist, listDirectory, pathIsSymbolicLink)
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
    firstchunk :: BS.ByteString,
    lastchunk :: BS.ByteString
  }

-- don't compare by filepath!
instance Eq File where
  (File _ fs1 firstchunk1 lastchunk1) == (File _ fs2 firstchunk2 lastchunk2) =
    (fs1, firstchunk1, lastchunk1) == (fs2, firstchunk2, lastchunk2)

instance Ord File where
  compare (File _ fs1 firstchunk1 lastchunk1) (File _ fs2 firstchunk2 lastchunk2) =
    compare (fs1, firstchunk1, lastchunk1) (fs2, firstchunk2, lastchunk2)

instance Show File where
  show (File fp _ _ _) = show fp

findDuplicates :: [FilePath] -> IO [[File]]
findDuplicates filenames = do
  files <- mapM loadFile filenames
  pure $ filter (\x -> 1 < length x) $ group (sort files)

checkFullDuplicates :: [FilePath] -> IO [[FilePath]]
checkFullDuplicates fps = do
  allContents <- mapM BS.readFile fps
  let pairs = zip fps allContents
      sorted = sortOn snd pairs
      dups = filter (\x -> length x > 1) $ groupOn snd sorted
      res = (fst <$>) `fmap` dups
  pure res

loadFile :: FilePath -> IO File
loadFile fp = do
  (fsize, firstchunk, lastchunk) <- withFile fp ReadMode getChunks
  pure $ File fp fsize firstchunk lastchunk

chunkSize ::
  -- | chunkSize is 4096 so NVMe drives will be especially happy
  Int
chunkSize = 4 * 1024

getChunks :: Handle -> IO (Integer, BS.ByteString, BS.ByteString)
getChunks h = do
  fsize <- hFileSize h
  begin <- BS.hGet h chunkSize
  hSeek h SeekFromEnd (fromIntegral chunkSize) -- [TODO] needs to be read from filesize - filesize % 4096
  end <- BS.hGet h chunkSize
  pure (fsize, begin, end)

-- get all the FilePath values
getFileNames :: FilePath -> IO [FilePath]
getFileNames curDir = do
  names <- listDirectory curDir
  let names' = (curDir </>) <$> names
  names'' <- filterM saneFile names'
  files <- forM names'' $ \path -> do
    let path' = curDir </> path
    exists <- doesDirectoryExist path'
    if exists
      then getFileNames path'
      else pure $ pure path'
  pure $ concat files

saneFile :: FilePath -> IO Bool
saneFile fp = ifM (doesPathExist fp) (not <$> pathIsSymbolicLink fp) (pure False)
