<<<<<<< HEAD
# HW05
## Deadline: 23:59 12.03.2024

1. (1 point) Make your solution of HW04 into a stack project.
2. (1 points) Replace the associative list with a `Map` from `Data.Map.Strict` in the `containers` package.
3. (4 points) Replace your hand-written test suite with a `tasty` test suite. Ensure good coverage.

If you start working on this assignment before I've finished checking your HW04 assignment, you can switch to a project in HW04 branch. This way you won't need to synchronize between branches. No need to create HW05 and open PR in this case either. 
=======
# HW04
## Deadline 23:59 05.03.2024

1. (1 point) What is the `Functor` instance of `Either`? Implement the instance and prove that the functor laws hold.
2. (1 point) What is the `Functor` instance of an arrow type? Hint: consider the type `a -> b` in its prefix notation: `(->) a b`. Implement the instance and prove that the functor laws hold.
3. (1 point) Add `String` variables to the type of expressions from HWO3; make the type of constants to be a type parameter of `Expr`.
4. (2 points) Modify the evaluator so it works with the new type of constants and variables. Add the new parameter to `eval` -- an associative list which maps variable names into their numerical values. 
    * `eval :: Expr a -> [(String, a)] -> Either Error a`
    * Now a new type of error becomes possible. Modify `Error` type accordingly.
    * Make sure to update tests.
5. (2 points) Implement the function `simplify :: Expr a -> Expr a` that simplifies the expression according to common laws of arithmetics. Implement as many laws as you can think of. 
    * Example laws: `0 * 42 == 0`, `1 * x == x`.
    * Add tests.
7. (1 points) Make `Expr a` to be an instance of `Num`. You should be able to implement all of the functions in the minimal set except for `abs` and `signum`.
    * It should be possible to write `simplify $ 1 + 0 * (Var "x")`, where `Var` is the constructor for an single variable expression.
    * Yes, there is no going around variables, but you should not need to wrap numbers in anything.
>>>>>>> HW04
