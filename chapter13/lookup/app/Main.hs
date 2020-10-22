module Main where

import Control.Monad (when)
import Lib
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  -- Load command line arguments
  args <- getArgs
  -- If we don't have the right amount of args, give an error and abort
  when (length args /= 2) $ do
    putStrLn "Syntax: passwd-al filename uid"
    exitFailure

  -- Read the file lazily
  content <- readFile (head args)

  -- Compute the username in pure code
  let username = findByUid content (read (args !! 1))

  -- Display the result
  case username of
    Just x -> putStrLn x
    Nothing -> putStrLn "Could not find that UID"