module Main where

import System.Environment
import Takedouble
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, mbytes]
      | [(bytes, "")] <- reads mbytes,
        bytes >= 1 ->
        findDuplicates dir bytes
    _ -> do
      name <- getProgName
      printf "Something went wrong - please use ./%s <dir> <bytes>\n" name
