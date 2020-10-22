module Main where

import System.Environment
import Control.Monad(when)
import System.Exit
import Lib

main :: IO ()
main = do
    args <- getArgs

    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure

    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps
