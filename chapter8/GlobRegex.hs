module GlobRegex
    (
        globToRegex
    ,    matchesGlob
    ) where

import Text.Regex.Posix ((=~))
import Data.Char (toUpper, toLower)

globToRegex :: String -> Bool -> Either String String
globToRegex cs ci = case globToRegex' cs ci of
                            Left message -> Left message
                            Right pat -> Right $ '^' : pat ++ "$"

globToRegex' "" _ = Right ""
globToRegex' ('*':cs) ci = case globToRegex' cs ci of
                            Left message -> Left message
                            Right pat -> Right $ ".*" ++ pat
globToRegex' ('?':cs) ci = case globToRegex' cs ci of
                            Left message -> Left message
                            Right pat -> Right $ "." ++ pat
globToRegex' ('[':'!':c:cs) ci = case charClass cs ci of
                                    Left message -> Left message
                                    Right pat -> Right $ "[^" ++ caseInsens ci c ++ pat
globToRegex' ('[':c:cs) ci = case charClass cs ci of
                                    Left message -> Left message
                                    Right pat -> Right $ "[" ++ caseInsens ci c ++ pat
globToRegex' ('[':_) _ = Left "unterminated character class"
globToRegex' (c:cs) ci = case globToRegex' cs ci of
                            Left message -> Left message
                            Right pat -> Right $ escape c ci ++ pat 

escape :: Char -> Bool -> String
escape c ci
    |   c `elem` regexChars = '\\':[c]
    |   otherwise = caseInsens ci c
    where regexChars = "\\+()^$.{}|"

caseInsens :: Bool -> Char -> String
caseInsens False c = [c]
caseInsens _ c 
    | c `elem` ['A'..'Z'] = '[':c:toLower c:"]"
    | c `elem` ['a'..'z'] = '[':toUpper c: c :"]"
    | otherwise = [c]    

charClass :: String -> Bool -> Either String String
charClass (']':cs) ci = case globToRegex' cs ci of
                            Left message -> Left message
                            Right pat -> Right $ ']' : pat 
charClass (c:cs) ci = case charClass cs ci of
                        Left message -> Left message
                        Right pat -> Right $ caseInsens ci c ++ pat
charClass [] _ = Left "unterminated character class"

matchesGlob :: FilePath -> String -> Bool -> Either String Bool
matchesGlob name pat ci = case globToRegex pat ci of
                            Left message -> Left message
                            Right pattern -> Right $ name =~ pattern