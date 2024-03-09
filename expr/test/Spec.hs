{-# OPTIONS_GHC -Wno-type-defaults #-}
import Test.Tasty (testGroup, defaultMain, TestTree)
import EvalTests ( evalTests )
import SimplifyTests (simplifyTests)




tests :: TestTree
tests = testGroup "Expression Tests" [evalTests, simplifyTests]

main :: IO ()
main = defaultMain tests
