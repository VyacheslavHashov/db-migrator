module Database.Migrator where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M

type MgFolder = T.Text
type MgNumber = Int

data MigrationName = MigrationName (Maybe MgNumber) T.Text

data Migration = Migration
    { mgName :: MigrationName
    , mgContent :: B.ByteString
    } deriving (Show)

type MgMap = M.Map MgFolder Migration

-- | All errors
data Error
    -- | Migration exists in database but not on disk
    = Phantom
    -- | Cyclic dependency in migrations
    | CyclicDep
    -- | Migration exists on disk but was not applied in database
    | Unapplied
    -- | Folder has more than one base migration
    | MoreOnceBase
    -- | Two different migration in folder have the same migration as previous
    -- In that case merge is need
    | NeedMerge
    -- | Migration with that name already exists
    | DuplicateName

data ParseError
    = NoHeader
    | InvalidFormat

-- | Migration name should contain only alpha characters and digits
isValidMgName :: T.Text -> Bool
isValidMgName = undefined

parseMgName :: T.Text -> MigrationName
parseMgName = undefined


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

