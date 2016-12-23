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
-- TODO test for file header
-- TODO test for reading migrations from disk

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


-- Helpers
assertRight :: (Show e, Show a) => Either e a -> Expectation
assertRight = (`shouldSatisfy` isRight)

assertLeft :: (Show e, Show a) => Either e a -> Expectation
assertLeft  = (`shouldSatisfy` isLeft)

