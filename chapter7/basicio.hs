--main = do
--    putStrLn "Hey fella, what's ya name?"
--    name <- getLine
--    putStrLn $ "Nice to meet you " ++ name

main =
    putStrLn "Hey fella, what's ya name?" >>
    getLine >>=
    (\name -> putStrLn $ "Nice to meet you " ++ name)