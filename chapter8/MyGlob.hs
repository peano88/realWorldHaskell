
-- Rules:
--- * ---> n carachters
--- ? ---> 1 caracter
--- c ---> c
--- [Class] ---> 1 char of Class
--- [!Class] ---> 1 char outside Class

--leaving aside escaped

data Rule = Simple Char 
           | Interval [Char]
           | NInterval [Char]
           | Any
           | Empty 
           deriving (Eq,Show)

fit :: Rule -> Char -> Bool
fit (Simple x) c = x == c
fit (Interval xs) c = c `elem` xs
fit (NInterval xs) c = not $ c `elem` xs
fit Any _ = True
fit Empty _ = False

data Matcher =  Trivial Rule
                | Concat Matcher Matcher
                | Option Matcher
                deriving (Eq,Show)

match :: Matcher -> String -> Bool
match (Trivial r) (c:_) = fit r c
match (Concat m1 m2) (c:cs) = match m1 [c] && match m2 cs
match (Option m) cs@(c:cs') = match m cs || match (Option m) cs'
match _ "" = False

parse :: String -> Either String Matcher
parse ('[':'!':cs) =  case break (==']') cs of
                        (_,"") -> Left "Invalid pattern"
                        (cls, rest) -> case parse $ tail rest of
                            Left message -> Left message
                            Right m -> Right $ Trivial (NInterval cls) </> m   
parse ('[':cs) =  case break (==']') cs of
                        (_,"") -> Left "Invalid pattern"
                        (cls, rest) -> case parse $ tail rest of
                            Left message -> Left message
                            Right m -> Right $ Trivial (Interval cls) </> m   
parse ('?':cs) = case parse cs of
                    Left message -> Left message
                    Right m -> Right $ Trivial Any </> m
parse ('*':cs) = case parse cs of
                    Left message -> Left message
                    Right m -> Right $ Option m
parse (c:cs) = case parse cs of
                    Left message -> Left message
                    Right m -> Right $ Trivial (Simple c) </> m
parse [] = Right $ Trivial Empty                   

(</>) :: Matcher -> Matcher -> Matcher
m </> (Trivial Empty) = m
m1 </> m2 = Concat m1 m2
                    
matchesGlob :: String -> String -> Either String Bool
matchesGlob pattern name = case parse pattern of
                            Left message -> Left message
                            Right m -> Right $ match m name