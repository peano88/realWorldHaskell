
doSomethingWith :: String -> String 
doSomethingWith "" = "Hey fella, I asked your name. Now I am pissed"
doSomethingWith input = "Really nice to meet you " ++ input ++
                        ". Dunno if you know it, but you name consists of " ++ show (length input) ++ " characters"

main :: IO ()
main = do
        putStrLn "Hey fella, what is your name?"
        inputStr <- getLine
        let outputStr = doSomethingWith inputStr
        putStrLn outputStr