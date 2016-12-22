import Database.Migrator

import Test.Hspec
import Data.Either

main :: IO ()
main = hspec $ do
    testParsingHeaders


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


-- Helpers
assertRight :: (Show e, Show a) => Either e a -> Expectation
assertRight = (`shouldSatisfy` isRight)

assertLeft :: (Show e, Show a) => Either e a -> Expectation
assertLeft  = (`shouldSatisfy` isLeft)

