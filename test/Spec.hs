-- Testing dependencies
import Test.DocTest(doctest) 
import Test.QuickCheck(quickCheck) 
import Test.Hspec(Spec, hspec, describe, shouldBe, it) 
import Test.Hspec.QuickCheck(prop) 

-- Functions to test
import Roll -- Rolling functions

main :: IO ()
main = do
    putStrLn "Doctests:"
    doctest ["-isrc", "app/Main.hs"]
    putStrLn "\nHspec tests:"
    hspec $ do
        testSquaredInteger
        testLengthCheck
        testSwap
        testRollRowLeft
        testRollRowsHelper
        testRollHelper
        testRoll
        

-- | testRoll test for roll function
testRoll :: Spec
testRoll =
    describe "Tests for roll function" $ do
        it "Left roll 3x3 grid" $ do
            roll "left" [1..9] `shouldBe` [1,6,9,2,5,8,3,4,7]
        it "Left roll 5x5 grid" $ do
            roll "left" [1..25] `shouldBe` [1,10,15,20,25,4,9,14,19,24,3,8,13,18,23,2,7,12,17,22,5,6,11,16,21]
        it "Right roll 3x3 grid" $ do
            roll "right" [1..9] `shouldBe` [7,4,3,8,5,2,9,6,1]
        it "Right roll 5x5 grid" $ do
            roll "right" [1..25] `shouldBe` [21,16,11,6,5,22,17,12,7,2,23,18,13,8,3,24,19,14,9,4,25,20,15,10,1]
        it "Roll no direction 3x3 grid" $ do
            roll "" [1..9] `shouldBe` []
        it "Roll right no grid" $ do
            roll "right" ([]::[Int]) `shouldBe` []

-- | testRollRowsHelper test for rollRowsHelper function
testRollRowsHelper :: Spec
testRollRowsHelper =
    describe "\nTests for rollRowsHelper function" $ do
        it "rollRowsHelper left 3x3 grid" $ do
           rollRowsHelper [1..9] True [0..2] 3 `shouldBe` [1,4,7,2,5,8,3,6,9]
        it "rollRowsHelper left 5x5 grid" $ do
            rollRowsHelper [1..25] True [0..4] 5 `shouldBe` [1,6,11,16,21,2,7,12,17,22,3,8,13,18,23,4,9,14,19,24,5,10,15,20,25]
        it "rollRowsHelper right 3x3 grid" $ do
            rollRowsHelper [1..9] False (reverse [0..2]) 3 `shouldBe` [7,4,1,8,5,2,9,6,3]
        it "rollRowsHelper right 5x5 grid" $ do
            rollRowsHelper [1..25] False (reverse [0..4]) 5 `shouldBe` [21,16,11,6,1,22,17,12,7,2,23,18,13,8,3,24,19,14,9,4,25,20,15,10,5]
        {- Add checking for empty grids or ranges? range should be of len rowLen, grid should be of len rowLen^2
        it "rollRowsHelper left no grid" $ do
            rollRowsHelper ([]::[Int]) False [0..2] 3 `shouldBe` []
        it "rollRowsHelper left no range 3x3 grid" $ do
            rollRowsHelper [1..9] False ([]::[Int]) 3 `shouldBe` []
        -}
        
testRollRowLeft :: Spec
testRollRowLeft =
    describe "\nTests for rollRowLeft function" $ do
        it "rollRowLeft 3x3 grid first row" $ do
           rollRowLeft 3 0 0 [1..9] `shouldBe` [1,4,7]
        it "rollRowLeft left 5x5 grid first row" $ do
            rollRowLeft 5 0 0 [1..25] `shouldBe` [1,6,11,16,21]

testLengthCheck :: Spec
testLengthCheck =
    describe "\nTests for lengthCheck function" $ do
        it "lengthCheck valid 3x3 grid" $ do
           lengthCheck [1..9] `shouldBe` 3
        it "lengthCheck valid 5x5 grid" $ do
            lengthCheck [1..25] `shouldBe` 5
        it "lengthCheck invalid (too small) grid" $ do
            lengthCheck [1..4] `shouldBe` 0
        it "lengthCheck invalid (not odd number) grid" $ do
            lengthCheck [1..16] `shouldBe` 0
        it "lengthCheck invalid (not squareable into an integer) grid" $ do
            lengthCheck [1..37] `shouldBe` 0

testSquaredInteger :: Spec
testSquaredInteger =
    describe "\nTests for squaredInteger function" $ do
        it "nine is 3 to the power of two" $ do
           squaredInteger 9 `shouldBe` 3
        it "16 is four to the power of two" $ do
            squaredInteger 16 `shouldBe` 4
        it "25 is five to the power of two" $ do
            squaredInteger 25 `shouldBe` 5
        it "Rounds the squared number if it doesn't return an integer" $ do
            squaredInteger 14 `shouldBe` 4
        it "Handles zero value gracefully" $ do
            squaredInteger 0 `shouldBe` 0
        it "Handles negatives gracefully" $ do
            squaredInteger (-25) `shouldBe` 0

testSwap :: Spec
testSwap =
    describe "\nTests for swap function" $ do
        it "3x3 grid should swap item one and three" $ do
            swap [1..9] `shouldBe` [3,2,1,4,5,6,7,8,9]
        it "5x5 grid should swap item one and five" $ do
            swap [1..25] `shouldBe` [5,2,3,4,1,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
        it "4x4 grid is invalid and should return an empty array" $ do
            swap [1..16] `shouldBe` []
        it "empty input should produce empty output" $ do
            swap ([]::[Int]) `shouldBe` []

testRollHelper :: Spec
testRollHelper =
    describe "\nTests for rollHelper function" $ do
        it "3x3 grid should swap item one and three" $ do
            swap [1..9] `shouldBe` [3,2,1,4,5,6,7,8,9]
        it "5x5 grid should swap item one and five" $ do
            swap [1..25] `shouldBe` [5,2,3,4,1,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
        it "4x4 grid is invalid and should return an empty array" $ do
            swap [1..16] `shouldBe` []
        it "empty input should produce empty output" $ do
            swap ([]::[Int]) `shouldBe` []