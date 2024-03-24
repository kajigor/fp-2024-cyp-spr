{-# OPTIONS_GHC -Wno-type-defaults #-}
import Test.Tasty (testGroup, defaultMain, TestTree)
import EvalTests ( evalTests )


tests :: TestTree
tests = testGroup "Expression Tests" [evalTests]

main :: IO ()
main = defaultMain tests