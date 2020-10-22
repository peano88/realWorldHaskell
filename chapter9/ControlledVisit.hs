module ControlledVisit (
    Info(..), getInfo, getUsefulContents, isDirectory, traverse
) where

import           System.Directory               ( Permissions(..)
                                                , getDirectoryContents
                                                , getPermissions
                                                , getModificationTime
                                                )
import           Data.Time.Clock                ( UTCTime(..) )
import           Control.Monad                  ( mapM
                                                , liftM
                                                , forM
                                                )
import           Data.Maybe                     ( maybe )
import           Control.Exception              ( handle
                                                , bracket
                                                )
import           System.IO                      ( IOMode(..)
                                                , hClose
                                                , hFileSize
                                                , openFile
                                                )
import           Prelude                 hiding ( traverse )
import System.Posix.Types (UserID(..), CUid(..))
import System.Posix.Files (getFileStatus, fileOwner)
import           System.FilePath                ( (</>) )

data Info = Info {
    infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    , infoOwner :: Maybe UserID
} deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a))
                     (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms    <- maybeIO (getPermissions path)
    size     <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    status <- maybeIO (getFileStatus path)
    let owner = maybe (CUid 0) fileOwner status
    return (Info path perms size modified (Just owner))

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return $ filter (`notElem` [".", ".."]) names

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names    <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

childrenFirst :: [Info] -> [Info]
childrenFirst (parent : children) = children ++ [parent]
childrenFirst []                  = []

traverseAndFilter
    :: ([Info] -> [Info]) -> ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseAndFilter order filter = traverse (filter . order)
