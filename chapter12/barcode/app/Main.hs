module Main where

import           Parse.Parse
import           Barcode
import           System.Environment             ( getArgs )
import qualified Data.ByteString.Lazy.Char8    as L
import           Control.Monad                  ( forM_ )

main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \arg -> do
        e <- parse parseRawPPM <$> L.readFile arg
        case e of
            Left  err    -> print $ "error" ++ err
            Right pixmap -> print $ findEAN13 pixmap
