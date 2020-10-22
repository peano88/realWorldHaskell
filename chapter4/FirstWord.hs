import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of 
                [input,output] -> interactWith function input output
                _ -> error "2 arguments required"

firstWords :: String -> String
firstWords text = unlines $ map (\row -> let ws = words row 
                                in if null ws then "" else head ws) $ lines text

myFunction = firstWords