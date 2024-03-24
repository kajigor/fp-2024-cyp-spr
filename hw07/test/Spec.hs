{-# OPTIONS_GHC -Wno-type-defaults #-}
import Test.Tasty (testGroup, defaultMain, TestTree)
import EvalTests ( evalTests )
import ParserTests (parserTests)


tests :: TestTree
tests = testGroup "Expression Tests" [
    evalTests,
    parserTests]

main :: IO ()
main = defaultMain tests