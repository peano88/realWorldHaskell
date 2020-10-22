import           System.Directory               ( Permissions(..)
                                                , getPermissions
                                                , getModificationTime
                                                )
import           Data.Time.Clock                ( UTCTime(..) )
import           RecursiveContents              ( getRecursiveContents )
import           Control.Monad                  ( filterM )
import           System.IO                      ( IOMode(..)
                                                , hClose
                                                , hFileSize
                                                , openFile
                                                )
import           Control.Exception              ( handle
                                                , bracket
                                                )
import           Data.Maybe                     ( maybe )
import           System.FilePath                ( takeExtension )

type Predicate
    =  FilePath       -- path to directory
    -> Permissions  -- permisions
    -> Maybe Integer -- file size (Nothing if not a file)
    -> UTCTime    -- last modified
    -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path =
    handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer))
        $ bracket (openFile path ReadMode) hClose
        $ \h -> do
              size <- hFileSize h
              return (Just size)


betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
        perms    <- getPermissions name
        size     <- getFileSize name
        modified <- getModificationTime name
        return (p name perms size modified)

type InfoP a
    =  FilePath       -- path to directory
    -> Permissions  -- permisions
    -> Maybe Integer -- file size (Nothing if not a file)
    -> UTCTime    -- last modified
    -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ s _ = maybe (-1) id s

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k
equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 =
    (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

(==?) = equalP
(&&?) = andP
(>?) = greaterP

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
