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
    testParsingHeaders
    testParserDependency
    testParserHeader

parse :: Parser a -> T.Text -> Either (P.ParseError Char P.Dec) a
parse = flip P.parse ""

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

testParserDependency :: Spec
testParserDependency = describe "parserDependency" $ do
    it "can parse right migration id" $
        parse parserDependency "base.0000" `shouldParse`
            MigrationId (MgFolder "base") (MgNumber 0)
    it "can parse right migration id with description" $
        parse parserDependency "base.0000_zero" `shouldParse`
            MigrationId (MgFolder "base") (MgNumber 0)
    it "should fail on invalid inputs" $ do
        parse parserDependency `shouldFailOn` "base0000"
        parse parserDependency `shouldFailOn` "base.zero"

testParserHeader :: Spec
testParserHeader = describe "parserDepList" $ do
    it "can parse single entry" $
        parse parserHeader "  -- cross-deps: base.0000\n" `shouldParse`
            [MigrationId (MgFolder "base") (MgNumber 0)]
    it "can parse multi entries" $
        parse parserHeader
            "  -- cross-deps: base.0000, internal.0001\n"
        `shouldParse`
            [ MigrationId (MgFolder "base") (MgNumber 0)
            , MigrationId (MgFolder "internal") (MgNumber 1)]

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

