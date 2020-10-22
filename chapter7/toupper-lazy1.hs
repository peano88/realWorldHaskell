import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
        inh <- openFile "input.txt" ReadMode
        outh <- openFile "output.txt" WriteMode
        contents <- hGetContents inh
        hPutStr outh (map toUpper contents) 
        hClose inh
        hClose outh
