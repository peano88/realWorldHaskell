module Parse.Parse where

import qualified Data.ByteString.Lazy          as L
import           Data.Int                       ( Int64(..) )
import           Data.Char                      ( chr
                                                , isDigit
                                                , isSpace
                                                )
import           Data.Word                      ( Word8(..) )
import           Data.List.Split                ( chunksOf )
import           Parse.PNM                            ( Greymap(..)
                                                , Gmap(..)
                                                , ImageData(..)
                                                )

data ParseState = ParseState {
    string :: L.ByteString
    ,offset :: Int64
} deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = case runParse parser (ParseState initState 0) of
    Left  error       -> Left error
    Right (result, _) -> Right result

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err =
    Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where
    chainedParser initState = case runParse firstParser initState of
        Left errMessage -> Left errMessage
        Right (firstResult, newState) ->
            runParse (secondParser firstResult) newState

-- (ByteString, offset) -> Either String (Word8,(ByteString,offset)) 
parseByte :: Parse Word8
parseByte = getState ==> \initState -> case L.uncons (string initState) of
    Nothing                -> bail "no more input"
    Just (byte, remainder) -> putState newState ==> \_ -> identity byte
      where
        newState  = initState { string = remainder, offset = newOffset }
        newOffset = offset initState + 1

instance Functor Parse where
    fmap f parser = parser ==> \result -> identity (f result)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte
-- Word8 -> Char -> (ByteString, offset) -> Either String (Word8,(ByteString,offset)) ->(ByteString, offset) -> Either String (Char,(ByteString,offset))

peekByte :: Parse (Maybe Word8)
peekByte = fmap fst . L.uncons . string <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp -> if mp == Just True
    then parseByte ==> \b -> (b :) <$> parseWhile p
    else identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits -> if null digits
    then bail "no more input"
    else
        let n = read digits
        in  if n < 0 then bail "integer overflow" else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> const f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n = getState ==> \st ->
    let n'     = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st'    = st { offset = offset st + L.length h, string = t }
    in  putState st'
            ==>& assert (L.length h == n') "end of input"
            ==>& identity h

parseRawPGM = parseWhileWith w2c notWhite ==> \header ->
    skipSpaces
        ==>& assert (header == "P5") "invalid raw header"
        ==>& parseNat
        ==>  \width -> skipSpaces ==>& parseNat ==> \height ->
                 skipSpaces ==>& parseNat ==> \maxGrey ->
                     parseByte
                         ==>& parseBytes (size maxGrey width height)
                         ==>  \bitmap ->
                                  identity (Greymap width height maxGrey bitmap)
  where
    notWhite = (`notElem` " \r\n\t")
    size maxGrey width height =
        if maxGrey < 256 then width * height else 2 * width * height

parseNats :: Int -> Parse [Int]
parseNats 0 = identity []
parseNats n = parseNat ==> \m -> skipSpaces ==>& ((m :) <$> parseNats (n - 1))

parsePlainPGM = parseWhileWith w2c notWhite ==> \header ->
    skipSpaces
        ==>& assert (header == "P2") "invalid raw header"
        ==>& parseNat
        ==>  \width -> skipSpaces ==>& parseNat ==> \height ->
                 skipSpaces
                     ==>& parseNat
                     ==>  \maxGrey ->
                              parseByte
                                  ==>& parseNats (width * height)
                                  ==>  \imageData ->
                                           identity
                                               (Gmap
                                                   width
                                                   height
                                                   maxGrey
                                                   ( ASCII
                                                   $ chunksOf width imageData
                                                   )
                                               )
    where notWhite = (`notElem` " \r\n\t")


parseImage :: String -> Int -> Int -> Int -> Parse ImageData
parseImage "P5" maxGrey width height = Binary
    <$> parseBytes (size maxGrey width height)
  where
    size maxGrey width height =
        if maxGrey < 256 then width * height else 2 * width * height
parseImage "P2" maxGrey width height =
    ASCII . chunksOf width <$> parseNats (width * height)
parseImage _ _ _ _ = bail "header not supported"

parsePGM = parseWhileWith w2c notWhite ==> \header ->
    skipSpaces ==>& parseNat ==> \width ->
        skipSpaces ==>& parseNat ==> \height ->
            skipSpaces ==>& parseNat ==> \maxGrey ->
                parseByte
                    ==>& parseImage header maxGrey width height
                    ==>  \imageData ->
                             identity (Gmap width height maxGrey imageData)
    where notWhite = (`notElem` " \r\n\t")
