import System.IO
import Data.Char (toUpper)

main :: IO ()
main =
    hSetBuffering stdin NoBuffering >> 
    interact (map toUpper)