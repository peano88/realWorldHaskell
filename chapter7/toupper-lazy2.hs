import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
        input <- readFile "input.txt"
        writeFile "output.txt" (map toUpper input)
