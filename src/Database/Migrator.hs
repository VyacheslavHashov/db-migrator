{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}

module Database.Migrator where

import Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.State
import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.Word
import Data.List (break, stripPrefix)
import System.FilePath
import System.Directory
import Control.Monad.Except
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
-- import qualified System.FilePath.Find as F
import qualified System.PosixCompat.Files as F

-- | The sequental number of a migration.
newtype MgNumber = MgNumber Word64
    deriving (Show, Eq, Ord, Read)

-- | The folder name where a migration is located. Must contain only alphabet
-- chars, digits and '_', '-'
newtype MgFolder = MgFolder T.Text
    deriving (Show, Eq, Ord)

-- | The desription for a migration. Must contain only alphabet chars, digits
-- and '_', '-'
newtype MgDesc   = MgDesc T.Text
    deriving (Show, Eq, Ord)

data MigrationId = MigrationId MgFolder MgNumber
    deriving (Show, Eq, Ord)

-- TODO custom eq and ord
data Migration = Migration
    { mgFolder  :: MgFolder
    , mgNumber  :: MgNumber
    , mgDesc    :: MgDesc
    } deriving (Eq, Ord, Show)

newtype CrossDeps = CrossDeps [MgList]

data MgNode = MgNode Migration CrossDeps

type MgList = [MgNode]

newtype MgGraph = MgGraph (M.Map MgFolder MgList)

newtype MgIndex = MgIndex (M.Map Migration MgList)

data RawMgNode = RawMgNode Migration [MigrationId]
    deriving Show

-- Build a migration folder validating that it contains only
-- allowed symbols
makeMgFolder :: T.Text -> Maybe MgFolder
makeMgFolder t | valid t   = Just $ MgFolder t
               | otherwise = Nothing
  where
    valid = T.all (\c -> isAlphaNum c || c == '_' || c == '-')

-- Build a migration's description validating that it contains only
-- allowed symbols
makeMgDesc :: T.Text -> Maybe MgDesc
makeMgDesc t | valid t   = Just $ MgDesc t
             | otherwise = Nothing
  where
    valid = T.all (\c -> isAlphaNum c || c == '_' || c == '-')



migrationId :: Migration -> MigrationId
migrationId = undefined

migrationFullname :: Migration -> T.Text
migrationFullname mg =  T.pack (show $ mgNumber mg) <> "_"


buildPlan :: MgList -> State (S.Set Migration) [Migration]
buildPlan [] = pure []
buildPlan (MgNode x (CrossDeps deps):xs) = do
    b <- gets (x `S.notMember`)
    if b
        then do
            modify (S.insert x)
            (<> [x]) . concat <$> traverse buildPlan (xs:deps)
        else pure []


getPlan :: MgGraph -> [Migration]
getPlan (MgGraph g) =  evalState (fmap concat . traverse buildPlan $ M.elems g) S.empty

data BuildState = BuildState
    { bsRest :: M.Map MgFolder [RawMgNode]
    , bsIndex :: MgIndex
    }

buildGraph :: M.Map MgFolder [RawMgNode] -> (MgGraph, MgIndex)
buildGraph gr =
  let (a, s) = runState (traverse buildFolder gr) initialState
  in (MgGraph a, bsIndex s)
  where
    initialState = BuildState gr (MgIndex M.empty)
    buildFolder :: [RawMgNode] -> State BuildState MgList
    buildFolder [] = pure []
    buildFolder (RawMgNode mg _ : xs) = do
        let node = MgNode mg (CrossDeps [])
        (node:) <$> buildFolder xs

constructPlan :: MgGraph -> MgGraph -> [Migration] -> [Migration]
constructPlan = undefined

-- TODO count all the arguments
listMigrations :: MgGraph -> [MgFolder] -> M.Map MgFolder [Migration]
listMigrations (MgGraph gr) _ =  fmap (fmap extractMg) gr
  where
    extractMg (MgNode mg _) = mg

checkMigrations :: MgGraph -> Either () ()
checkMigrations = undefined

printMigrationList :: M.Map MgFolder [Migration] -> T.Text
printMigrationList = M.foldMapWithKey f
  where
    f (MgFolder folder) xs = folder <> ":\n"
        <> T.unlines (map migrationFullname xs )<> "\n"

printPlan :: [Migration] -> T.Text
printPlan = T.unlines . map migrationFullname

-- | All errors
data Error
    -- | Migration exists in database but not on disk
    = Phantom
    -- | Cyclic dependency in migrations
    | CyclicDep
    -- | Migration exists on disk but was not applied in database
    | Unapplied
    -- | Folder has more than one base migration
    | MultipleBase
    -- | Unknown dependency
    | UnknownDep
    -- | Two different migration in folder have the same migration as previous
    -- In that case merge is need
    | NeedMerge
    -- | Migration with that name already exists
    | DuplicateName
    -- | Migration file has no SQl commands
    | EmptyMigration
    deriving (Show)

-- | Error with migrations files
data MigrationFileError
    = InvalidFilename T.Text
    | InvalidHeader
    deriving (Show)

-- | Errors in database with migrations
data MigrationDatabaseError = MigrationDatabaseError
    deriving (Show)

-- | All warnings
data Warning
    -- | Subfolders are ignored
    = IgnoredSubfolder
    deriving (Show)


-- Config

-- | Reads config from config file
-- Also read options from args
-- Config contains:
--   host
--   port
--   database name
--   user
--   password
readConfig :: IO ()
readConfig = undefined

-- Reads migrations


readFromDB :: IO (M.Map MgFolder RawMgNode)
readFromDB = undefined

-- | Create table for migrations in database
createMigrationTable :: IO ()
createMigrationTable = undefined

-- | Apply migration in database
-- Should be atomic operation:
-- execution migration file
-- append migration metainfo in table

applyMigration :: IO ()
applyMigration = undefined

-----------------------------------------------------------
-- File system
-----------------------------------------------------------

type FileM = ExceptT MigrationFileError IO

-- reads all the migrations from the disk
-- TODO dont count empty dirs
readFromDisk :: FileM (M.Map MgFolder [RawMgNode])
readFromDisk = do
    dir <- liftIO getCurrentDirectory
    folders <- liftIO $ listDirectory dir >>= filterM isDirectory
    M.fromList <$> traverse (\f -> (\a -> (MgFolder (T.pack f),a)) <$> listFiles f) folders

-- | List files in directory with extensions .sql or .pgsql
listFiles :: FilePath -> FileM [RawMgNode]
listFiles folder = do
    files <- liftIO $ listDirectory folder >>=
                      filterM (isMigrationFile . (folder </>))
    forM files $ \f -> do
        let name = takeBaseName f
        (number, desc) <- parseFilename name
        let mg = Migration (MgFolder $ T.pack folder) number desc
        mgids <- parseHeader =<< liftIO (B.readFile (folder </> f))
        pure $ RawMgNode mg mgids

isDirectory :: FilePath -> IO Bool
isDirectory = fmap F.isDirectory . F.getFileStatus

isMigrationFile :: FilePath -> IO Bool
isMigrationFile path = (isSQL &&) <$> isFile
  where
    isSQL  =  map toLower (takeExtension path) `elem` [".sql", ".pgsql"]
    isFile = F.isRegularFile <$> F.getFileStatus path

-- | Parses a filename in the parts of a migration name.
--
-- Filename must start with a number followed by an optional description
-- separated by the '_' symbol.
parseFilename
    :: MonadError MigrationFileError m => String -> m (MgNumber, MgDesc)
parseFilename name =
    let (rnumber, rdesc) = break (== '_') name
    in (,) <$> readNumber rnumber <*> readDesc rdesc
  where
    throwFilenameError = throwError $ InvalidFilename $ T.pack name
    readDesc ""        = pure $ MgDesc ""
    readDesc ['_']     = throwFilenameError
    readDesc ('_':xs)  = maybe throwFilenameError pure $ makeMgDesc $ T.pack xs
    readNumber = maybe throwFilenameError (pure . MgNumber) . readMaybe

-- | Parses a single dependency
-- format : <folder>.<number>_<desc>
-- TODO desc should be optional
-- TODO right validation and tests
parseDependency
    :: MonadError MigrationFileError m => B.ByteString -> m MigrationId
parseDependency s = do
    let s' = BC.takeWhile (not . isSpace) $ BC.dropWhile isSpace s
        (folder, rest) = BC.break (== '.') s'
        (rnumber, desc) = BC.break (== '_') rest
    when (B.null folder) $ throwError InvalidHeader
    case readMaybe (tail $ BC.unpack rnumber) of
        Nothing -> throwError InvalidHeader
        Just n -> pure $ MigrationId (MgFolder $ decodeUtf8 folder )
                                     (MgNumber n)

-- | Read header form migration file
-- header must contain deps list
parseHeader
    :: MonadError MigrationFileError m => B.ByteString -> m [MigrationId]
parseHeader s = do
    let s' = BC.dropWhile isSpace s
    case BC.stripPrefix "-- cross-deps:" s' of
    -- There are no deps
        Nothing -> pure []
        Just rest -> traverse parseDependency $ BC.split ',' rest

-----------------------------
-- TEST
-----------------------------
a1 = MgNode (Migration (MgFolder "a") (MgNumber 1) (MgDesc "a1") ) $
        CrossDeps []
a2 = MgNode (Migration (MgFolder "a") (MgNumber 2) (MgDesc "a2") ) $
        CrossDeps []
a3 = MgNode (Migration (MgFolder "a") (MgNumber 3) (MgDesc "a3") ) $
        CrossDeps []
a4 = MgNode (Migration (MgFolder "a") (MgNumber 4) (MgDesc "a4") ) $
        CrossDeps [drop 2 mb]
a5 = MgNode (Migration (MgFolder "a") (MgNumber 5) (MgDesc "a5") ) $
        CrossDeps []
b1 = MgNode (Migration (MgFolder "b") (MgNumber 1) (MgDesc "b1") ) $
        CrossDeps []
b2 = MgNode (Migration (MgFolder "b") (MgNumber 2) (MgDesc "b2") ) $
        CrossDeps [mc]
b3 = MgNode (Migration (MgFolder "b") (MgNumber 3) (MgDesc "b3") ) $
        CrossDeps []
b4 = MgNode (Migration (MgFolder "b") (MgNumber 4) (MgDesc "b4") ) $
        CrossDeps [drop 2 mc]
c1 = MgNode (Migration (MgFolder "c") (MgNumber 1) (MgDesc "c1") ) $
        CrossDeps []
c2 = MgNode (Migration (MgFolder "c") (MgNumber 2) (MgDesc "c2") ) $
        CrossDeps []
c3 = MgNode (Migration (MgFolder "c") (MgNumber 3) (MgDesc "c3") ) $
        CrossDeps []
c4 = MgNode (Migration (MgFolder "c") (MgNumber 4) (MgDesc "c4") ) $
        CrossDeps []
c5 = MgNode (Migration (MgFolder "c") (MgNumber 5) (MgDesc "c5") ) $
        CrossDeps []

ma = reverse [a1, a2, a3, a4, a5]
mb = reverse [b1, b2, b3, b4]
mc = reverse [c1, c2, c3, c4, c5]

graph :: MgGraph
graph = MgGraph $ M.fromList [(MgFolder "A", ma), (MgFolder "B", mb), (MgFolder "C", mc)]

readAndPrintList :: IO ()
readAndPrintList = do
    r <- runExceptT readFromDisk
    case r of
        Left _ -> error "bad"
        Right g -> T.putStr . printMigrationList . flip listMigrations [] . fst $ buildGraph g

readAndPrintPlan :: IO ()
readAndPrintPlan = do
    r <- runExceptT readFromDisk
    case r of
        Left _ -> error "bad"
        Right g -> T.putStr . printPlan . getPlan . fst $ buildGraph g

