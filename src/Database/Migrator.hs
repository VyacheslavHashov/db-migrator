{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TypeFamilies #-}
{-# language ScopedTypeVariables #-}

module Database.Migrator where

import Data.Monoid
import Data.Foldable
import Data.List (sortOn)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String (IsString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Traversable (for)
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
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Text as P
import qualified Text.Megaparsec.Lexer as P (integer, skipLineComment)
import Text.Megaparsec.Prim (MonadParsec)
import Hasql.Query   as H
import Hasql.Connection   as H
import Hasql.Session as H
import Hasql.Encoders as HE
import Hasql.Decoders as HD

-- | The sequental number of a migration.
newtype MgNumber = MgNumber Word64
    deriving (Show, Eq, Ord, Read)

-- | The folder name where a migration is located.
newtype MgFolder = MgFolder T.Text
    deriving (Show, Eq, Ord, IsString)

-- | The desription for a migration.
newtype MgDesc   = MgDesc T.Text
    deriving (Show, Eq, Ord, IsString)

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
    -- | Unknown dependency
    | UnknownDep
    -- | Two different migration in folder have the same migration as previous
    -- In that case merge is need
    | NeedMerge
    -- | Migration file has no SQl commands
    | EmptyMigration
    deriving (Show)

-- | Error with migrations files
data MigrationFileError
    = InvalidFilename FilePath
    | InvalidFoldername FilePath
    -- TODO must be contain place and error description
    | InvalidHeader
    -- | Migration with that name already exists
    | DuplicateName
    -- | Migration with expected name does not exist
    | MissedMigration
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

-----------------------------------------------------------
-- Database
-----------------------------------------------------------

readFromDB :: IO (M.Map MgFolder RawMgNode)
readFromDB = undefined

readRawFromDB :: IO (M.Map MgFolder RawMgNode)
readRawFromDB = undefined


readQuery :: H.Query () [RawMgNode]
readQuery = H.statement sql encoder decoder True
  where
    sql =
        "SELECT m.folder, m.number, m.description, dep.dep_list          \
        \    FROM migrator.migration as m                                \
        \    LEFT JOIN                                                   \
        \    (SELECT folder, number, array_agg(ROW(on_folder, on_number))\
        \        as dep_list                                             \
        \        FROM migrator.cross_dependency                          \
        \        GROUP BY (folder, number)                               \
        \        ORDER BY (folder, number)) as dep                       \
        \    ON m.folder = dep.folder AND m.number = dep.number          \
        \    ORDER BY folder;"
    encoder = HE.unit
    decoder = HD.rowsList $ do
        mg <- Migration <$> (MgFolder <$> HD.value HD.text)
                        <*> (MgNumber . fromIntegral  <$> HD.value HD.int4)
                        <*> (MgDesc <$> HD.value HD.text)
        deps <- fmap (fromMaybe []) . HD.nullableValue . HD.array .
                HD.arrayDimension replicateM . HD.arrayValue . HD.composite $
                    MigrationId <$> HD.compositeValue (MgFolder <$> HD.text)
                                <*> HD.compositeValue
                                    (MgNumber . fromIntegral <$>  HD.int4)
        pure $ RawMgNode mg deps

-- | Create table for migrations in database
-- TODO choose schema name
createMigrationTable :: H.Session ()
createMigrationTable = H.sql sql
  where
    sql = "\
        \ CREATE SCHEMA IF NOT EXISTS migrator;                     \
        \ CREATE TABLE migrator.migration (                         \
        \     folder      text        NOT NULL,                     \
        \     number      integer     NOT NULL CHECK (number >= 0), \
        \     description text        NOT NULL,                     \
        \     applied_at  timestamptz NOT NULL DEFAULT NOW(),       \
        \     content     text        NOT NULL,                     \
        \                                                           \
        \     PRIMARY KEY (folder, number)                          \
        \ );                                                        \
        \ CREATE TABLE IF NOT EXISTS migrator.cross_dependency (    \
        \     folder text       NOT NULL,                           \
        \     number integer    NOT NULL,                           \
        \     on_folder text    NOT NULL,                           \
        \     on_number integer NOT NULL,                           \
        \                                                           \
        \     FOREIGN KEY (folder, number)                          \
        \         REFERENCES migrator.migration(folder, number),    \
        \     FOREIGN KEY (on_folder, on_number)                    \
        \         REFERENCES migrator.migration(folder, number),    \
        \     CHECK (folder <> on_folder)                           \
        \ );"

-- | Apply migration in database
-- Should be atomic operation:
-- execution migration file
-- append migration metainfo in table

applyMigration :: IO ()
applyMigration = undefined

-----------------------------------------------------------
-- File Storage
-----------------------------------------------------------

type FileM = ExceptT MigrationFileError IO

-- TODO switch from [RawMgNode] to NonEmpty RawMgNode since list always has
-- at least one item

readFromDisk :: FileM (M.Map MgFolder [RawMgNode])
readFromDisk = validateRawNodes =<< readRawFromDisk

-- TODO right docs
  -- TODO tests for all cases
  -- check that all the numbers go sequentally
  -- check there are no duplicates
-- TODO must check that cross dep does not refer to the same folder
-- Checks the consistency of the raw nodes and returns nodes in the correct
-- order for build graph
-- items in the list go in order:
--   first entry should be applied first
-- Numbers of the migrations in should be sequence
-- 0000, 0001, 0002, 0003 etc.
validateRawNodes
    :: M.Map MgFolder [RawMgNode] -> FileM (M.Map MgFolder [RawMgNode])
validateRawNodes = traverse $ \nodes -> do
    let nodesSorted = sortOn getNumber nodes
        -- TODO errors must contain name of migrations
    for (zip nodesSorted numberSeq) $ \(node, eNumber) ->
        case getNumber node `compare` eNumber of
            LT -> throwError MissedMigration
            GT -> throwError MissedMigration
            EQ -> pure node
  where
    getNumber (RawMgNode mg _) = mgNumber mg
    numberSeq = MgNumber <$> [0 .. ]

-- TODO maybe raise warning if the items is not a folder and config file
-- | Reads all the migrations from the disk, but not check for consistency
readRawFromDisk :: FileM (M.Map MgFolder [RawMgNode])
readRawFromDisk = do
    dir <- liftIO getCurrentDirectory
    folders <- liftIO $ listDirectory dir >>= filterM isDirectory
    fold <$> traverse listFiles folders

-- TODO maybe raise warnings if the extension is not SQL
-- TODO maybe raise warnings if the item is not a file
-- | List all the files in directory with extensions .sql or .pgsql
listFiles :: FilePath -> FileM (M.Map MgFolder [RawMgNode])
listFiles folderPath = do
    folder <- runParser
        (const . throwError $ InvalidFoldername folderPath)
        parserFolderEnd "" folderPath

    files <- liftIO $ listDirectory folderPath >>=
                      filterM (isMigrationFile . (folderPath </>))
    mgs <- forM files $ \f -> do
        let name = takeBaseName f
        (number, desc) <- runParser
            (const . throwError $ InvalidFilename (folderPath </> f))
            parserFilenameEnd "" name

        mgids <- liftIO (T.readFile (folderPath </> f)) >>= runParser
            (const ( throwError InvalidHeader))
            parserHeader ""

        pure $ RawMgNode (Migration folder number desc) mgids

    -- Empty directory should be ignored
    pure $ if null mgs
        then M.empty
        else M.singleton folder mgs
  where
    runParser errHandler p name content = case P.parse p name content of
        Left (e :: P.ParseError Char P.Dec) -> errHandler e
        Right v                             -> pure v

isDirectory :: FilePath -> IO Bool
isDirectory = fmap F.isDirectory . F.getFileStatus

isMigrationFile :: FilePath -> IO Bool
isMigrationFile path = (isSQL &&) <$> isFile
  where
    isSQL  =  map toLower (takeExtension path) `elem` [".sql", ".pgsql"]
    isFile = F.isRegularFile <$> F.getFileStatus path

-----------------
-- Parsers
-----------------

-- | Parses a folder name that should contains only alphabet characters, digits
-- and '_', '-'
parserFolder :: (MonadParsec e s m, P.Token s ~ Char) => m MgFolder
parserFolder = MgFolder . T.pack <$> P.some allowedChar
  where allowedChar = P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-')

-- | Parses a migration description that should contains only alphabet
-- characters, digits and '_', '-'
parserDesc :: (MonadParsec e s m, P.Token s ~ Char) => m MgDesc
parserDesc = MgDesc . T.pack <$> P.some allowedChar
  where allowedChar = P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-')

-- | Parses a migration number that is unsigned integer
parserNumber :: (MonadParsec e s m, P.Token s ~ Char) => m MgNumber
parserNumber = MgNumber . fromInteger <$> P.integer

-- | Parses a filename
-- format : <number>[_<desc>]
parserFilename :: (MonadParsec e s m, P.Token s ~ Char) => m (MgNumber, MgDesc)
parserFilename = (,) <$> parserNumber
                     <*> P.option (MgDesc "") (P.char '_' *> parserDesc)

-- | Parses a filename on the end of input
parserFilenameEnd :: (MonadParsec e s m, P.Token s ~ Char) => m (MgNumber, MgDesc)
parserFilenameEnd = parserFilename <* P.eof

-- | Parses a folder name on the end of input
parserFolderEnd :: (MonadParsec e s m, P.Token s ~ Char) => m MgFolder
parserFolderEnd = parserFolder <* P.eof

-- | Parses a migration id
-- format : <folder>.<number>[_<desc>]
parserMigrationId :: (MonadParsec e s m, P.Token s ~ Char) => m MigrationId
parserMigrationId = do
    folder <- parserFolder
    P.char '.'
    number <- parserNumber
    P.optional (P.char '_' *> parserDesc)
    pure $ MigrationId folder number

-- | Parses the header of a migration.
-- The line with a dependency list should be the first non-empty line in the
-- file. Otherwise it is assumed that the migration has no dependencies
--
-- Header's format: -- cross-deps: <dependency> (, <dependency)*
parserHeader :: (MonadParsec e s m, P.Token s ~ Char) => m [MigrationId]
parserHeader = emptyLines *>
               (P.try (prefix *> depList) <|>
                P.notFollowedBy prefix *> pure [])
  where
    emptyLine  = P.notFollowedBy prefix *>
                 skipSpace *> P.optional (P.skipLineComment "--" )
    emptyLines = P.sepEndBy emptyLine (P.skipSome P.eol)
    prefix     = skipSpace *> P.string "--" *> skipSpace *>
                 P.string "cross-deps:"
    depList    = P.sepBy1 (skipSpace *> parserMigrationId <* skipSpace)
                 (P.char ',') <* (P.eof <|> void P.eol)

-- | Just as @spaces@ from Megaparsec but does not consume newlines and
-- carriage returns
skipSpace :: (MonadParsec e s m, P.Token s ~ Char) => m ()
skipSpace = P.skipMany (P.oneOf whitespaces P.<?> "white space")
  where whitespaces = ['\t', '\f', '\v',' ']


-----------------------------
-- TEST
-----------------------------
a1 = MgNode (Migration  "a" (MgNumber 1) "a1" ) $ CrossDeps []
a2 = MgNode (Migration  "a" (MgNumber 2) "a2" ) $ CrossDeps []
a3 = MgNode (Migration  "a" (MgNumber 3) "a3" ) $ CrossDeps []
a4 = MgNode (Migration  "a" (MgNumber 4) "a4" ) $ CrossDeps [drop 2 mb]
a5 = MgNode (Migration  "a" (MgNumber 5) "a5" ) $ CrossDeps []
b1 = MgNode (Migration  "b" (MgNumber 1) "b1" ) $ CrossDeps []
b2 = MgNode (Migration  "b" (MgNumber 2) "b2" ) $ CrossDeps [mc]
b3 = MgNode (Migration  "b" (MgNumber 3) "b3" ) $ CrossDeps []
b4 = MgNode (Migration  "b" (MgNumber 4) "b4" ) $ CrossDeps [drop 2 mc]
c1 = MgNode (Migration  "c" (MgNumber 1) "c1" ) $ CrossDeps []
c2 = MgNode (Migration  "c" (MgNumber 2) "c2" ) $ CrossDeps []
c3 = MgNode (Migration  "c" (MgNumber 3) "c3" ) $ CrossDeps []
c4 = MgNode (Migration  "c" (MgNumber 4) "c4" ) $ CrossDeps []
c5 = MgNode (Migration  "c" (MgNumber 5) "c5" ) $ CrossDeps []

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

runQueryDb :: IO ()
runQueryDb = do
    rc <- H.acquire $ settings "" 5432 "v" "" "testdb"
    case rc of
        Left e -> print e
        Right c -> do
            r <- H.run (H.query () readQuery) c
            -- r <- H.run createMigrationTable c
            either print print r

