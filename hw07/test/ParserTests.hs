module ParserTests(parserTests) where
import Test.Tasty
import Utils(testParseExpression)
import Expr (Expr(..), BinaryOperator (..), UnaryOperator (..))

parseConstTests :: TestTree
parseConstTests = testGroup "Const" [
    testParseExpression "Simple" "10" (Right ("", Const 10)),
    testParseExpression "Negative number" "-10" (Left "Predicate is not satisfied"),
    testParseExpression "Floating point number" "10abc" (Left "Expected space or end of input"),
    testParseExpression "Extra symbols after number" "10 foo" (Right (" foo", Const 10)),
    testParseExpression "Extra symbols before number" " 10" (Left "Predicate is not satisfied")
  ]

parseVarTests :: TestTree
parseVarTests = testGroup "Variable" [
    testParseExpression "Simple" "foo" (Right ("", Var "foo")),
    testParseExpression "Starts from digit" "1foo" (Left "Expected space or end of input"),
    testParseExpression "Has digits" "foo123bar456" (Right ("", Var "foo123bar456")),
    testParseExpression "Extra symbols after var" "foo123bar456 extra" (Right (" extra", Var "foo123bar456"))
  ]

parseAddTests :: TestTree
parseAddTests = testGroup "Add" [
    testParseExpression "Simple" "+ 1 1" (Right ("", BinOp Plus (Const 1) (Const 1))),
    testParseExpression "With var" "+ 1 foo" (Right ("", BinOp Plus (Const 1) (Var "foo"))),
    testParseExpression "Extra symbols after" "+ 1 1 foo" (Right (" foo", BinOp Plus (Const 1) (Const 1))),
    testParseExpression "Double" "+ + 1 1 1" (Right ("", BinOp Plus (BinOp Plus (Const 1) (Const 1)) (Const 1))),
    testParseExpression "One argument" "+ 1" (Left "Predicate is not satisfied"),
    testParseExpression "Zero argument" "+" (Left "Predicate is not satisfied")
    ]

parseSubtractTests :: TestTree
parseSubtractTests = testGroup "Subtract" [
    testParseExpression "Simple" "- 1 1" (Right ("", BinOp Minus (Const 1) (Const 1))),
    testParseExpression "With var" "- 1 foo" (Right ("", BinOp Minus (Const 1) (Var "foo"))),
    testParseExpression "Extra symbols after" "- 1 1 foo" (Right (" foo", BinOp Minus (Const 1) (Const 1))),
    testParseExpression "Double" "- - 1 1 1" (Right ("", BinOp Minus (BinOp Minus (Const 1) (Const 1)) (Const 1))),
    testParseExpression "One argument" "- 1" (Left "Predicate is not satisfied"),
    testParseExpression "Zero argument" "-" (Left "Predicate is not satisfied")
    ]

parseMultiplyTests :: TestTree
parseMultiplyTests = testGroup "Multiply" [
    testParseExpression "Simple" "* 1 1" (Right ("", BinOp Multiply (Const 1) (Const 1))),
    testParseExpression "With var" "* 1 foo" (Right ("", BinOp Multiply (Const 1) (Var "foo"))),
    testParseExpression "Extra symbols after" "* 1 1 foo" (Right (" foo", BinOp Multiply (Const 1) (Const 1))),
    testParseExpression "Double" "* * 1 1 1" (Right ("", BinOp Multiply (BinOp Multiply (Const 1) (Const 1)) (Const 1))),
    testParseExpression "One argument" "* 1" (Left "Predicate is not satisfied"),
    testParseExpression "Zero argument" "*" (Left "Predicate is not satisfied")
    ]

parseDivideTests :: TestTree
parseDivideTests = testGroup "Divide" [
    testParseExpression "Simple" "/ 1 1" (Right ("", BinOp Divide (Const 1) (Const 1))),
    testParseExpression "With var" "/ 1 foo" (Right ("", BinOp Divide (Const 1) (Var "foo"))),
    testParseExpression "Extra symbols after" "/ 1 1 foo" (Right (" foo", BinOp Divide (Const 1) (Const 1))),
    testParseExpression "Double" "/ / 1 1 1" (Right ("", BinOp Divide (BinOp Divide (Const 1) (Const 1)) (Const 1))),
    testParseExpression "One argument" "/ 1" (Left "Predicate is not satisfied"),
    testParseExpression "Zero argument" "/" (Left "Predicate is not satisfied")
    ]

parsePowTests :: TestTree
parsePowTests = testGroup "Power" [
    testParseExpression "Simple" "^ 1 1" (Right ("", BinOp Power (Const 1) (Const 1))),
    testParseExpression "With var" "^ 1 foo" (Right ("", BinOp Power (Const 1) (Var "foo"))),
    testParseExpression "Extra symbols after" "^ 1 1 foo" (Right (" foo", BinOp Power (Const 1) (Const 1))),
    testParseExpression "Double" "^ ^ 1 1 1" (Right ("", BinOp Power (BinOp Power (Const 1) (Const 1)) (Const 1))),
    testParseExpression "One argument" "^ 1" (Left "Predicate is not satisfied"),
    testParseExpression "Zero argument" "^" (Left "Predicate is not satisfied")
    ]

parseSquareTests :: TestTree
parseSquareTests = testGroup "Square" [
    testParseExpression "Simple" "sqrt 1" (Right ("", UnOp Square (Const 1))),
    testParseExpression "With var" "sqrt foo" (Right ("", UnOp Square (Var "foo"))),
    testParseExpression "Extra symbols after" "sqrt 1 foo" (Right (" foo", UnOp Square (Const 1))),
    testParseExpression "Double" "sqrt sqrt 1" (Right ("", UnOp Square (UnOp Square (Const 1)))),
    testParseExpression "Zero argument" "sqrt" (Left "Predicate is not satisfied")
    ]

parseCombinedTests :: TestTree
parseCombinedTests = testGroup "Combined" [
    testParseExpression "Respect order right" "+ 123 * 45 6" (Right ("", BinOp Plus (Const 123) (BinOp Multiply (Const 45) (Const 6)))),
    testParseExpression "Respect order left" "- / 123 45 6" (Right ("", BinOp Minus (BinOp Divide (Const 123) (Const 45)) (Const 6)))
    ]

parserTests :: TestTree
parserTests = testGroup "Parser tests" [
    parseConstTests,
    parseVarTests,
    parseAddTests,
    parseSubtractTests,
    parseMultiplyTests,
    parseDivideTests,
    parsePowTests,
    parseSquareTests,
    parseCombinedTests
    ]