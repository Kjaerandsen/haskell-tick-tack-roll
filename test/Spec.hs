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

testRoll :: Spec
testRoll =
    describe "Tests for roll function" $ do
        it "Left roll 3x3 grid" $ do
            roll "left" [1..9] `shouldBe` [3,6,9,2,5,8,1,4,7]
        it "Left roll 5x5 grid" $ do
            roll "left" [1..25] `shouldBe` [5,10,15,20,25,4,9,14,19,24,3,8,13,18,23,2,7,12,17,22,1,6,11,16,21]
        it "Right roll 3x3 grid" $ do
            roll "right" [1..9] `shouldBe` [9,6,3,8,5,2,7,4,1]
        it "Right roll 5x5 grid" $ do
            roll "right" [1..25] `shouldBe` [25,20,15,10,5,24,19,14,9,4,23,18,13,8,3,22,17,12,7,2,21,16,11,6,1]
        it "Roll no direction 3x3 grid" $ do
            roll "" [1..9] `shouldBe` []
        it "Roll right no grid" $ do
            roll "right" ([]::[Int]) `shouldBe` []


