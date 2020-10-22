import           ControlledVisit                ( Info(..)
                                                , getUsefulContents
                                                , getInfo
                                                , isDirectory
                                                )
import           Data.Char                      ( toLower )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                , takeExtension
                                                )

data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return $ unwrap endSeed
 where
  fold seed subpath = getUsefulContents subpath >>= walk seed

  walk seed (name : names) = do
    let path' = path </> name
    info <- getInfo path'
    case iter seed info of
      done@(Done _) -> return done
      Skip seed'    -> walk seed' names
      Continue seed'
        | isDirectory info -> do
          next <- fold seed' path
          case next of
            done@(Done _) -> return done
            seed''        -> walk (unwrap seed'') names
        | otherwise -> walk seed' names
  walk seed _ = return (Continue seed)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
  | length paths == 3                 = Done paths
  | isDirectory info && takeFileName path == ".git" = Skip paths
  | extension `elem` [".jpg", ".png"] = Continue (path : paths)
  | otherwise                         = Continue paths
 where
  extension = map toLower (takeExtension path)
  path      = infoPath info

countDirectories :: Iterator Integer
countDirectories count info =
  if isDirectory info then Continue $ count + 1 else Done count

type Condition a = a -> Info -> Bool
type SeedTrns a = a -> Info -> Iterate a

type MaybeIterator seed = seed -> Info -> Maybe (Iterate seed)

(<+>) :: MaybeIterator seed -> MaybeIterator seed -> MaybeIterator seed
(<+>) miFirst miSecond seed info = case miFirst seed info of
  Just it -> Just it
  Nothing -> miSecond seed info

toIterator :: MaybeIterator seed -> Iterator seed
toIterator maybeIt seed info = maybe (Continue seed) id (maybeIt seed info)

toto :: Condition a -> SeedTrns a -> MaybeIterator a
toto cond seedTrns seed info | cond seed info = Just $ seedTrns seed info
                             | otherwise      = Nothing

isDirectoryCond :: Condition a
isDirectoryCond _ = isDirectory

increments :: SeedTrns Integer
increments count _ = Continue $ count + 1

countDirectories2 :: Iterator Integer
countDirectories2 = toIterator $ toto isDirectoryCond increments
