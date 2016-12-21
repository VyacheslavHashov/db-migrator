{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}

module Database.Migrator where

import Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.State
import Data.Char (isAlphaNum, isSpace)
import Data.Word
import Data.List (break, stripPrefix)
import System.FilePath
import System.Directory
import Control.Monad.Except
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
-- import qualified System.FilePath.Find as F
import qualified System.PosixCompat.Files as F

newtype MgNumber = MgNumber Word64
    deriving (Show, Eq, Ord, Read)

newtype MgFolder = MgFolder T.Text
    deriving (Show, Eq, Ord)

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

-- instance Show Migration where
--     show m = T.unpack $ mgFolder m <> "." <> mgName m

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


getPath :: MgGraph -> [Migration]
getPath (MgGraph g) =  evalState (fmap concat . traverse buildPlan $ M.elems g) S.empty


buildGraph :: M.Map MgFolder RawMgNode -> (MgGraph, MgIndex)
buildGraph = undefined

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

-- -- | Migration name should contain only alpha characters, dash and digits
-- -- and its length must be non-zero
-- isValidMgName :: T.Text -> Bool
-- isValidMgName t = T.all (\c -> isAlphaNum c || c == '_') t && not (T.null t)

-- -- TODO change to Either
-- -- TODO test it
-- parseFilename :: String -> Maybe Migration
-- parseFilename s = undefined


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

---------
-- File system


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
-- Filename must start with a number followed by an optional description
-- separated by the '_' symbol.
parseFilename
    :: MonadError MigrationFileError m => String -> m (MgNumber, MgDesc)
parseFilename name = do
    let (rnumber, rdesc) = break (== '_') name
        desc = MgDesc . T.pack . fromMaybe "" $ stripPrefix "_" rdesc
    case readMaybe rnumber of
        Nothing -> throwError $ InvalidFilename $ T.pack name
        Just n  -> pure (MgNumber n, desc)

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

-- TEST
a1 = MgNode (Migration (MgFolder "a") (MgNumber 1) (MgDesc "a1") ) $ CrossDeps []
a2 = MgNode (Migration (MgFolder "a") (MgNumber 2) (MgDesc "a2") ) $ CrossDeps []
a3 = MgNode (Migration (MgFolder "a") (MgNumber 3) (MgDesc "a3") ) $ CrossDeps []
a4 = MgNode (Migration (MgFolder "a") (MgNumber 4) (MgDesc "a4") ) $ CrossDeps [drop 2 mb]
a5 = MgNode (Migration (MgFolder "a") (MgNumber 5) (MgDesc "a5") ) $ CrossDeps []
b1 = MgNode (Migration (MgFolder "b") (MgNumber 1) (MgDesc "b1") ) $ CrossDeps []
b2 = MgNode (Migration (MgFolder "b") (MgNumber 2) (MgDesc "b2") ) $ CrossDeps [mc]
b3 = MgNode (Migration (MgFolder "b") (MgNumber 3) (MgDesc "b3") ) $ CrossDeps []
b4 = MgNode (Migration (MgFolder "b") (MgNumber 4) (MgDesc "b4") ) $ CrossDeps [drop 2 mc]
c1 = MgNode (Migration (MgFolder "c") (MgNumber 1) (MgDesc "c1") ) $ CrossDeps []
c2 = MgNode (Migration (MgFolder "c") (MgNumber 2) (MgDesc "c2") ) $ CrossDeps []
c3 = MgNode (Migration (MgFolder "c") (MgNumber 3) (MgDesc "c3") ) $ CrossDeps []
c4 = MgNode (Migration (MgFolder "c") (MgNumber 4) (MgDesc "c4") ) $ CrossDeps []
c5 = MgNode (Migration (MgFolder "c") (MgNumber 5) (MgDesc "c5") ) $ CrossDeps []

ma = reverse [a1, a2, a3, a4, a5]
mb = reverse [b1, b2, b3, b4]
mc = reverse [c1, c2, c3, c4, c5]

graph :: MgGraph
graph = MgGraph $ M.fromList [(MgFolder "A", ma), (MgFolder "B", mb), (MgFolder "C", mc)]

