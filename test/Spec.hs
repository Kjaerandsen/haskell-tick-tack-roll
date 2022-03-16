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
        testRoll
        testRollRowsHelper

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
        
