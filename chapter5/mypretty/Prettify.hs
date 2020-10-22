module Prettify
    (
        Doc,
        empty,
        char,
        text,
        double,
        line,
        (<>),
        hcat,
        fsep,
        compact,
        pretty,
        punctuate,
        fill,
        nest

    ) where

import Prelude hiding ((<>))

data Doc = Empty
        | Char Char
        | Text String
        | Line
        | Concat Doc Doc
        | Union Doc Doc
        deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f Empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

compact :: Doc -> String
compact x = transform [x]
    where   transform [] = ""
            transform (d:ds) =
                case d of
                    Empty           -> transform ds
                    Char c          -> c : transform ds
                    Text s          -> s ++ transform ds
                    Line            -> '\n' : transform ds
                    a `Concat` b    -> transform (a:b:ds)
                    _ `Union` b     -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where   best col (d:ds) =
                case d of
                    Empty           -> best col ds
                    Char c          -> c : best (col + 1) ds
                    Text s          -> s ++ best (col + length s) ds
                    Line            -> '\n' : best 0 ds
                    a `Concat` b    -> best col (a:b:ds)
                    a `Union` b     -> nicest col (best col (a:ds))
                                          (best col (b:ds))
            best _ _ = ""
            nicest col a b  | (width - least) `fits` a  = a
                            | otherwise                 = b
                where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0  = False
w `fits` ""         = True
w `fits` ('\n':_)   = True
w `fits` (c:cs)     = (w - 1) `fits` cs

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []  = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

fill :: Int -> Doc -> Doc
fill width x = spaces 0 [x]
    where   spaces col (d:ds) =
                case d of
                    Empty           -> spaces col ds
                    Char c          -> Char c <> spaces (col + 1) ds
                    Text s          -> Text s <> spaces (col + length s) ds
                    Line            -> let toFill = width - col in
                        if toFill > 0 then Text (replicate toFill ' ') <> Line <> spaces 0 ds
                        else Line <> spaces 0 ds
                    a `Concat` b    -> spaces col (a:b:ds)
                    a `Union` b     -> spaces col (a:ds) `Union` spaces col (b:ds)   
            spaces _ _ = Empty

-- not checking if the brackets are balanced            
nest :: Int -> Doc -> Doc
nest ind x = indent [] [x] 
    where   indent pStack (d:ds) = 
                case d of
                    Empty           -> indent pStack ds
                    Char c          -> charHandling c pStack ds 
                    Text s          -> Text s <> indent pStack ds
                    Line            -> Line <> tab pStack <> indent pStack ds
                    a `Concat` b    -> indent pStack (a:b:ds)
                    a `Union` b     -> indent pStack (a:ds) `Union` indent pStack (b:ds)   
            indent _ _ = Empty
            tab parentheses = Text (replicate (length parentheses * ind) ' ')
            isParOpen c = c `elem` ['(', '[', '{']
            isParClose c = c `elem` [')', ']', '}']
            charHandling c pStack ds
                | isParOpen c = Char c <> Line <> tab (c:pStack) <> indent (c:pStack) ds
                | isParClose c = Line <> tab (tail pStack) <> Char c <> Line <> tab (tail pStack) <> indent (tail pStack) ds 
                | otherwise = Char c <> indent pStack ds
