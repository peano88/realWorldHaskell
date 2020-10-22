howManySmallerThanI :: [Int] -> [Int]
howManySmallerThanI xs = map snd $ foldl updateIfGreater [] xs
  where
    updateIfGreater [] x = [(x, 0)]
    updateIfGreater ((y,c) : ys) x =
        let newCounter = if y > x then c + 1 else c
        in  (y, newCounter) : updateIfGreater ys x


howManySmallerThanI2 :: [Int] -> [Int]
howManySmallerThanI2 xs = reverse $ foldl singleStep [] withIndex
  where
      withIndex =  zip xs [0..]
      singleStep acc (x,i) = length (filter (\y -> x > fst y && i < snd y) withIndex) : acc