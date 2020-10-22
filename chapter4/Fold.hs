myFoldl :: (b -> a -> b) -> b -> [a] -> b


myFoldl f z xs = foldr step id xs z
-- this is the same of (foldr step id xs) $ z
    where step x g b = g (f b x)
-- g is the accumulator (the function which applied to the z will get the end result)
-- it his a function b -> b hence the b in the definition
