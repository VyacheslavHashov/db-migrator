{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}

module Database.Migrator where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.State
import Data.Char (isAlphaNum)
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

newtype MigrationId = MigrationId T.Text
    deriving (Show, Eq, Ord)

-- TODO custom eq and ord
data Migration = Migration
    { mgFolder  :: MgFolder
    , mgNumber  :: MgNumber
    , mgDesc    :: MgDesc
    } deriving (Eq, Ord)

newtype CrossDeps = CrossDeps [MgList]

data MgNode = MgNode Migration CrossDeps

type MgList = [MgNode]

newtype MgGraph = MgGraph (M.Map MgFolder MgList)

newtype MgIndex = MgIndex (M.Map Migration MgList)

data RawMgNode = RawMgNode Migration [MigrationId]

-- instance Show Migration where
--     show m = T.unpack $ mgFolder m <> "." <> mgName m

migrationId :: Migration -> MigrationId
migrationId = undefined

migrationFullname :: Migration -> T.Text
migrationFullname = undefined


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

listMigrations :: MgGraph -> [MgFolder] -> M.Map MgFolder [Migration]
listMigrations = undefined

checkMigrations :: MgGraph -> Either () ()
checkMigrations = undefined

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
readFromDisk :: FileM (M.Map MgFolder RawMgNode)
readFromDisk = do
    dir <- liftIO getCurrentDirectory
    folders <- liftIO $ listDirectory dir >>= filterM isDirectory
    liftIO $ print folders
    traverse listFiles folders
    undefined

-- | List files in directory with extensios .sql or .pgsql
-- listFiles :: FilePath -> ExceptT MigrationFileError IO [RawMgNode]
listFiles :: FilePath -> ExceptT MigrationFileError IO ()
listFiles path = do
    files <- liftIO $ listDirectory path >>=
                      filterM (isMigrationFile . (path </>))
    let names = takeBaseName <$> files
    liftIO $ print names

isDirectory :: FilePath -> IO Bool
isDirectory = fmap F.isDirectory . F.getFileStatus

isMigrationFile :: FilePath -> IO Bool
isMigrationFile path = (isSQL &&) <$> isFile
  where
    isSQL  =  map toLower (takeExtension path) `elem` [".sql", ".pgsql"]
    isFile = F.isRegularFile <$> F.getFileStatus path

-- | Parse a filename in the parts of a migration name.
-- Filename must start with a number followed by an optional description
-- separated by the '_' symbol.
parseFilename :: String -> Except MigrationFileError (MgNumber, MgDesc)
parseFilename name = do
    let (rnumber, rdesc) = break (== '_') name
        desc = MgDesc . T.pack . fromMaybe "" $ stripPrefix "_" rdesc
    case readMaybe rnumber of
        Nothing -> throwError $ InvalidFilename $ T.pack name
        Just n  -> pure (MgNumber n, desc)

-- | Read header form migration file
-- header must contain deps list
parseHeader :: B.ByteString -> Except MigrationFileError [MigrationId]
parseHeader = undefined

