# HW03

## Deadline 23:59 27.02.2024

1. (1 point) Design an algebraic data type `Expr` for a binary expression. 
   * A number of type `Double` is an expression.
   * A square root of an expression is an expression. 
   * Binary operators `+`, `-`, `*`, `/`, and `^` can be used to create an expression

2. (1 point) Make `Expr` an instance of `Show` and `Eq`.

3. (1 point) Design an algebraic data type `Error` for possible errors which can happen while evaluating the expression (task 5). 

4. (1 point) Make `Error` an instance of `Show` and `Eq`. 

5. (2 point) Implement evaluator function `eval`. It may fail when taking a square root of an negative number or when dividing by zero. 

6. (1 point) Provide test cases for the evaluator: `cases`. Make sure running `main` prints nothing. 