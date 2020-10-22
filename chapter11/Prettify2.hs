
module Prettify2
    (
        Doc(..),
        empty,
        char,
        text,
        double,
        line,
        (<>),
        hcat,
        --fsep,
        --compact,
        --pretty,
        punctuate,
        --fill,
        --nest

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

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f Empty

hcat :: [Doc] -> Doc
hcat = fold (<>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []  = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds