{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}

module Database.Migrator where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.Char (isAlphaNum)
import Data.Word
import Data.List (break)

newtype MgNumber = MgNumber Word64
    deriving (Show, Eq, Ord)

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

-- | Error with migrations files
data MigrationFileError = MigrationFileError

-- | Errors in database with migrations
data MigrationDatabaseError = MigrationDatabaseError

-- | All warnings
data Warning
    -- | Subfolders are ignored
    = IgnoredSubfolder

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

readFromDisk :: IO (M.Map MgFolder RawMgNode)
readFromDisk = undefined

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

-- Check migrations

checkMigrations :: IO ()
checkMigrations = undefined

-- File system

-- | List files in directory with extensios .sql or .pgsql
listMigrations :: IO ()
listMigrations = undefined

-- | Extract name without extension
extractName :: IO ()
extractName = undefined

-- | Read header form migration file
-- header must contain deps list
parseHeader :: IO ()
parseHeader = undefined

