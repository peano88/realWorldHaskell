module Lib
  ( findByUid
  )
where

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split delimiter str =
  let (before, remainder) = span (/= delimiter) str
  in  before : case remainder of
        []   -> []
        rest -> split delimiter (tail rest)

splitLB :: Eq a => a -> [a] -> [[a]]
splitLB delimiter = foldr doIt []
 where
  doIt x ys
    | x == delimiter = [] : ys
    | otherwise = case ys of
      []         -> [[x]]
      (y' : ys') -> (x : y') : ys'

-- from a line like one of /etc/passwd get (uid,username) (no error handling)
parseline :: String -> (Integer, String)
parseline line =
  let tokens = split ':' line in (read (tokens !! 2), head tokens)

findByUid :: String -> Integer -> Maybe String
findByUid content uid =
  let users = map parseline . lines $ content in lookup uid users

