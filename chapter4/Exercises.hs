import Data.Char (digitToInt, isHexDigit)

asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs


asInt_foldl xs
    | all isHexDigit xs = Right $ foldl (\acc c -> acc*10 + digitToInt c) 0 xs
    | otherwise = Left "invalid format" 

concat_foldr :: [[a]] -> [a]
concat_foldr xs = foldr (++) [] xs

mTakeWhile :: (a->Bool) -> [a] -> [a]
mTakeWhile _ [] = []
mTakeWhile f (x:xs) = if f x then x:mTakeWhile f xs else []

-- using same technique of foldl == foldr
takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr f xs = foldr step id xs []
    where
        step x g ys
            |f x = x:g ys
            |otherwise = id ys

-- simpler            
takeWhile_foldr1 :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr1 p xs = foldr (\x xs -> if p x then x:xs else []) [] xs

-- It does not do the same :(
groupByFoldr :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldr p xs = foldr step [] xs
    where
        step x ys@(y:ys') 
            | any (p x) y = (x:y):ys'
            | otherwise = [x]:ys
        step x [] = [[x]]


groupByFoldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldl p xs = foldl step [] xs
    where
        step ys x
            | not $ null ys = if any (p x) (last ys) then init ys ++ [(last ys) ++ [x]] else ys ++ [[x]]
            | otherwise = ys ++ [[x]]

anyFold :: (a -> Bool) -> [a] -> Bool
anyFold p  = foldl (\res x -> p x || res) False

anyFoldr :: (a -> Bool) -> [a] -> Bool
anyFoldr p = foldr (\x res -> p x || res) False 

-- take n $ cycleFold does not work
cycleFold :: [a] -> [a]
cycleFold xs = foldr (\x l -> x:l) xs $ cycleFold xs

wordsFold :: String -> [String]
wordsFold = foldr step [""] 
    where 
        step c ys   
             | c == ' ' = "" : ys
             | otherwise = (c:head ys):tail ys

unlinesFold :: [String] -> String
unlinesFold = foldr step "" 
    where
        step row text = foldr (:) ('\n':text) row