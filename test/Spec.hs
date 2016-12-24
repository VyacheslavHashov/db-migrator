{-# language OverloadedStrings #-}
import Database.Migrator

import Test.Hspec
import Test.Hspec.Megaparsec
import Data.Either
import qualified Text.Megaparsec as P (parse, Dec, ParseError)
import Text.Megaparsec.Text (Parser)
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
    describe "Testing parsers" testParsers

-- | Helper to deduce Parser type
parse :: Parser a -> T.Text -> Either (P.ParseError Char P.Dec) a
parse = flip P.parse ""

testParsers :: Spec
testParsers = do
    testParserFilenameEnd
    testParserMigrationId
    testParserHeader

testParsingHeaders :: Spec
testParsingHeaders = describe "parseFilename" $ do
    it "can parse name as only digits" $
        assertRight $ parseFilename "0000"
    it "can parse name with description" $
        assertRight $ parseFilename "0000_zero"
    it "should fail on invalid names" $ do
        assertLeft $ parseFilename "0000zero"
        assertLeft $ parseFilename "zero"
        assertLeft $ parseFilename "000_zero second"
        assertLeft $ parseFilename "0000_"
        assertLeft $ parseFilename "0000_zero.second"

testParserFilenameEnd :: Spec
testParserFilenameEnd = describe "parserFilenameEnd" $ do
    it "can parse name as only digits" $
        parse parserFilenameEnd "0000" `shouldParse` (MgNumber 0, MgDesc "")
    it "can parse name with description" $
        parse parserFilenameEnd "0000_zero" `shouldParse` (MgNumber 0, MgDesc "zero")
    it "should fail on invalid names" $ do
        parse parserFilenameEnd  `shouldFailOn` "0000zero"
        parse parserFilenameEnd  `shouldFailOn` "zero"
        parse parserFilenameEnd  `shouldFailOn` "000_zero second"
        parse parserFilenameEnd  `shouldFailOn` "0000_"
        parse parserFilenameEnd  `shouldFailOn` "0000_zero.second"

testParserMigrationId :: Spec
testParserMigrationId = describe "parserMigrationId" $ do
    it "can parse right migration id" $
        parse parserMigrationId "base.0000" `shouldParse`
            MigrationId (MgFolder "base") (MgNumber 0)

    it "can parse right migration id with description" $
        parse parserMigrationId "base.0000_zero" `shouldParse`
            MigrationId (MgFolder "base") (MgNumber 0)

    it "should fail on invalid inputs" $ do
        parse parserMigrationId `shouldFailOn` "base0000"
        parse parserMigrationId `shouldFailOn` "base.zero"

testParserHeader :: Spec
testParserHeader = describe "parserHeader" $ do
    it "can parse single entry" $
        parse parserHeader "  -- cross-deps: base.0000\n" `shouldParse`
            [MigrationId (MgFolder "base") (MgNumber 0)]

    it "can parse multi entries" $
        parse parserHeader
            "  -- cross-deps: base.0000, internal.0001\n"
        `shouldParse`
            [ MigrationId (MgFolder "base") (MgNumber 0)
            , MigrationId (MgFolder "internal") (MgNumber 1)]

    it "can parse with empty lines before" $ do
        let s = T.unlines
                [ "  "
                , "\t  "
                , "--comment"
                , "--second"
                , "  -- cross-deps: base.0000" ]
        parse parserHeader s `shouldParse`
            [MigrationId (MgFolder "base") (MgNumber 0)]

    it "should parse only the first possible header" $ do
        let s = T.unlines
                [ "-- cross-deps: base.0000"
                , "-- cross-deps: internal.0001"]
        parse parserHeader s `shouldParse`
            [MigrationId (MgFolder "base") (MgNumber 0)]

    it "should ignore header after the non-empty lines" $
        parse parserHeader "SELECT 1 \n-- cross-deps: internal.0001\n"
            `shouldParse` []


    it "should return empty list if header does not exist" $
        parse parserHeader " -- comment \n SELECT 1;" `shouldParse` []

    it "should fail on input without comma as separator" $
        parse parserHeader `shouldFailOn`
            "  -- cross-deps: base.0000 internal.0001\n"
    it "should fail on input list after 'cross-deps' header" $
        parse parserHeader `shouldFailOn`
            "  -- cross-deps: \n"



-- TODO test for reading migrations from disk

-- Helpers
assertRight :: (Show e, Show a) => Either e a -> Expectation
assertRight = (`shouldSatisfy` isRight)

assertLeft :: (Show e, Show a) => Either e a -> Expectation
assertLeft  = (`shouldSatisfy` isLeft)

