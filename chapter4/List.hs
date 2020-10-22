mNull :: [a] -> Bool
mNull [] = True
mNull _ = False

-- copied without pity from the Prelude source
mAppend :: [a] -> [a] -> [a]
mAppend [] xs = xs
mappend (x:xs) ys = x:mAppend xs ys

mHead :: [a] -> a
mHead (x:xs) = x
mHead _ = error "Empty List"

mTail :: [a] -> [a]
mTail (x:xs) = xs
mTail _ = error "Empty List"

-- exactly like the standard, will behave very bad with infinite list
mLast :: [a] -> a
mLast [] = error "Empty List"
mLast (x:[]) = x
mLast (x:xs) = mLast xs

-- exactly like the standard, will behave very bad with infinite list
mInit :: [a] -> [a]
mInit [] = error "Empty List"
mInit (x:[]) = []
mInit (x:xs) = x:mInit xs 

mConcat :: [[a]] -> [a]
mConcat [] = []
mConcat (x:xs) = x ++ (mConcat xs)

mReverse :: [a] -> [a]
mReverse [] = []
mReverse (x:xs) = reverse xs ++ [x]

mAnd ::[Bool] -> Bool
mAnd [] = True
mAnd (x:xs) = x && (mAnd xs)

mOr :: [Bool] -> Bool
mOr [] = True
mOr (x:xs) = x || mOr xs

mAll :: (a->Bool) -> [a] -> Bool
mAll _ [] = True
mAll f (x:xs) = f x && mAll f xs
-- mAll f xs = mAnd . map f $ xs

mAny :: (a->Bool) -> [a] -> Bool
mAny _ [] = True
mAny f (x:xs) = f x || mAny f xs 

-- not safe for n < 0 : will take the whole list
mTake :: Int -> [a] -> [a]
mTake 0 _ = []
mTake _ [] = []
mTake n (x:xs) = x:mTake (n-1) xs

-- not safe for n < 0 : will drop the whole list
mDrop :: Int -> [a] -> [a]
mDrop 0 xs = xs
mDrop _ [] = []
mDrop n (x:xs) = mDrop (n-1) xs

mSplitAt :: Int -> [a] -> ([a],[a])
mSplitAt n xs = (mTake n xs, mDrop n xs)

mTakeWhile :: (a->Bool) -> [a] -> [a]
mTakeWhile _ [] = []
mTakeWhile f (x:xs) = if f x then x:mTakeWhile f xs else []

mDropWhile :: (a->Bool) -> [a] -> [a]
mDropWhile _ [] = []
mDropWhile f xs@(x:xs') = if f x then mDropWhile f xs' else xs

mBreak :: (a->Bool) -> [a] -> ([a], [a])
-- mBreak f xs = (mTakeWhile (not f) xs, mDropWhile (not f) xs) it is not efficient. truing to do better
mBreak f [] = ([],[])
mBreak f (x:xs)
    | f x = ([],x:xs)
    | otherwise = (x:ys,zs)
        where (ys,zs) = mBreak f xs

mElem :: (Eq a) => a -> [a] -> Bool
mElem _ [] = False
mElem y (x:xs)
    | y == x = True
    | otherwise = y `mElem` xs

mFilter :: (a -> Bool) -> [a] -> [a]
mFilter f [] = []
mFilter f (x:xs)
    | f x = x:mFilter f xs
    | otherwise = mFilter f xs

mIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
mIsPrefixOf [] _ = True
mIsPrefixOf _ [] = False
mIsPrefixOf (x:xs) (y:ys) 
    | x == y = mIsPrefixOf xs ys
    | otherwise = mIsPrefixOf (x:xs) ys

mZip :: [a] -> [b] -> [(a,b)]
mZip (x:xs) (y:ys) = (x,y):mZip xs ys
mZip _ _ = []

mZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
mZipWith f (x:xs) (y:ys) = f x y : mZipWith f xs ys
mZipWith _ _ _ = []

mLines :: String -> [String]
mLines [] = []
mLines row = let (word, rest) = mBreak (\c -> c == '\n') row
                in word : if null rest then [] else mLines (tail rest)