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
                

transposeWords :: String -> String
transposeWords = unlines . transpose . uniform . lines
    where
        --rows = lines text
        maxLength = maximum . (map length)
        uniform rows = map (\r -> let difference = maxLength rows - length r 
                                    in  if difference > 0 then append difference r else r) rows
        append 0 row = row                            
        append n row = append (n-1) row ++ " "
        transpose (row:[]) = map (flip (:)"") row                                
        transpose (row1:rows) = zipWith (\a b -> a:b) row1 (transpose rows)  

myFunction = transposeWords