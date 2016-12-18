{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}

module Database.Migrator where

import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.Tree
import Data.Char (isAlphaNum)

type MgNumber = Int

data Migration = Migration
    { mgFolder  :: T.Text
    , mgName    :: T.Text
    } deriving (Eq, Ord)

instance Show Migration where
    show m = T.unpack $ mgFolder m <> "." <> mgName m

type Deps = [Migration]
type Node = Tree

type MgGraph = [Node Migration]

traverseGraph
    :: MgGraph
    -> (S.Set Migration -> Migration -> Bool)
    -> State (S.Set Migration) [Migration]
traverseGraph graph f = concat <$> traverse go graph
  where
    go (Node x ts) = do
       s <- get
       if (x `S.notMember` s) && f s x
           then do
             modify (S.insert x)
             (<> [x]) . concat <$> mapM go ts
           else pure []

getPath :: MgGraph -> [Migration]
getPath g =  evalState (traverseGraph g (\_ _ -> True)) S.empty


makeGraph :: M.Map Migration Deps -> MgGraph
makeGraph = undefined


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

-- | All warnings
data Warning
    -- | Subfolders are ignored
    = IgnoredSubfolder

data ParseError
    = NoHeader
    | InvalidFormat

-- | Migration name should contain only alpha characters, dash and digits
-- and its length must be non-zero
isValidMgName :: T.Text -> Bool
isValidMgName t = T.all (\c -> isAlphaNum c || c == '_') t && not (T.null t)

-- parseMgName :: T.Text -> MigrationName
-- parseMgName = undefined


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

readFromDisk :: IO ()
readFromDisk = undefined

readFromDB :: IO ()
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

-- client functions

-- | Creates new empty migration
cliNew :: IO ()
cliNew = undefined

-- | List all migrations
cliList :: IO ()
cliList = undefined

-- | Merge migrations
cliMerge :: IO ()
cliMerge = undefined

-- | Apply migrations
cliApply :: IO ()
cliApply = undefined


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

