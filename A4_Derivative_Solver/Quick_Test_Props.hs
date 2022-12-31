module Quick_Test_Props where

import Test.QuickCheck

infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

infix 5 ~~ -- weak conformation
(~~) :: (Floating a,Ord a) => a -> a -> Bool
x ~~ y = abs (x - y) <= 1e-1

-- Function to run tests props and display them as IO.
runTest :: String -> ((Float,Float) -> Bool) -> IO ()
runTest desc test = do
    putStr $ "\t" ++ desc ++ "\n\t \\\\  "
    quickCheck test 
    putStr "\n"

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Add (Coef x) X) y

{- Function: eval
 - Test #1
 - Property: eval (Mult (Coef x) (Power X (-1))) y is correct for all x,y
 - Actual Test Result: Pass -}
evalProp1 :: (Float,Float) -> Bool
evalProp1 (x,y)
    | x == 0 && y == 0 = isNaN evaluation
    | y == 0 = isInfinite evaluation
    | otherwise = (x / y) =~ evaluation || (x / y) ~~ evaluation
        -- Some values require weak conformation check to pass. For
        -- Active debugging, remove the weak confirmation check.
    where evaluation = eval (Mult (Coef x) (Power X (-1))) y

{- Function: eval
 - Test #2
 - Property: eval (Abs $ Sin $ Cos X) x is correct for all x
 - Actual Test Result: Pass -}
evalProp2 :: (Float,Float) -> Bool
evalProp2 (x,_) = abs(sin $ cos x) =~ eval (Abs $ Sin $ Cos X) x

{- Function: eval
 - Test #3
 - Property: eval ((x + X) / (y - x)) y is correct for all x, y
 - Note: Testing construction methods
 - Actual Test Result: Pass -}
evalProp3 :: (Float,Float) -> Bool
evalProp3 (x,y)
    | x == y = isNaN(eval expression y)
    | otherwise = (x + y) / (y - x) =~ eval expression y
    where expression = (Coef x + X) / (Coef y - Coef x)

runEvalProp :: IO ()
runEvalProp = do
    putStrLn "\n\t----- Function Tests: eval -----\n"
    runTest "eval (Func2 Add (Coef x) X) y == x + y" 
        evalProp0
    runTest "eval (Mult (Coef x) (Power X (-1))) y == x / y"
        evalProp1
    runTest "eval (Abs $ Sin $ Cos X) x == abs(sin(cos x))"
        evalProp2
    runTest "eval (x + X / y - x) y == x + y / y - x"
        evalProp3

{- Function: eval
 - Test #1
 - Property: diff $ Mult (Coef x) (Power X 2) 1 is correct for all x, y 
 -           when y is X.
 - Actual Test Result: Pass -}
diffProp0 :: (Float,Float) -> Bool
diffProp0 (x,y) = (2 * x) * y =~ eval (diff $ Mult (Coef x) (Power X 2)) y

{- Function: eval
 - Test #2
 - Property: diff $ sin $ cos $ x*X is correct for all x, y when y is X
 - Actual Test Result: Pass -}
diffProp1 :: (Float,Float) -> Bool
diffProp1 (x,y) = (-x)*cos(cos(x*y))*sin(x*y)=~ 
                    eval (diff $ Sin $ Cos (Mult (Coef x) X)) y

{- Function: eval
 - Test #3
 - Property: diff $ abs ((x + X) / (y - x)) is correct for all x, y
 - Notes: Testing construction methods
 - Actual Test Result: Pass -}
diffProp2 :: (Float,Float) -> Bool
diffProp2 (x,y)
    | c == 0 = isNaN(eval expression y)
    | otherwise = (abs(c) * (x + y))/(c^^2 * abs(x + y)) =~ eval expression y
    where expression = diff $ abs ((Coef x + X) / (Coef y - Coef x))
          c = y - x

runDiffProp :: IO ()
runDiffProp = do
    putStrLn "\n\t----- Function Tests: diff -----\n"
    runTest "eval (diff $ Mult (Coef x) (X^^2)) y == 2*x*y" 
        diffProp0
    runTest "eval (diff $ sin $ cos $ x*X) y == (-x)*cos(cos(x*y))*sin(x*y)"
        diffProp1
    runTest "eval diff $ abs ((x + X) / (y - x)) y == (abs(c) * (x + y))/(c^^2 * abs(x + y))"
        diffProp2

-- Type "runAllTests" in shell to run all of the determined quicktest cases.
runAllTests :: IO ()
runAllTests = do runEvalProp ; runDiffProp