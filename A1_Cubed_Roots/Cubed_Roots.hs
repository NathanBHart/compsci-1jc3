module Cubed_Roots where

-- Cubed_Roots
-- Name: Nathan Hart
-- Date: Wednesday Sept 28 2022

-- Alternative exponentiation function
-- used for taking a valid cubed root
(***) :: Double -> Double -> Double
x *** y = if x >= 0 then x ** y else -( (-x) ** y)


{- Function: cubicQ -------------------------------------------------

    Description: Function that approximates value Q from values of a,
      b, and c.

    Input: Values a, b, and c as doubles from a cubic equation.

    Output: A double value approximately equivalent to the value Q -}

cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ( 3*a*c - b**2 ) / ( 9*a**2 )


{- Function: cubicR -------------------------------------------------

    Description: Function that approximates value R from values of a,
      b, c, and d.

    Input: Values a, b, c, and d as doubles from a cubic equation.

    Output: A double value approximately equivalent to the value R -}

cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ( 9*a*b*c - 27*d*a**2 - 2*b**3 ) / ( 54*a**3 ) 


{- Functions: cubicDisc ---------------------------------------------

    Description: Function that approximates the cubic discriminant
      from values of R and Q. 

    Input: Values R and Q as doubles.

    Output: A double value approximately equivalent to the cubic 
      discriminant                                                 -}

cubicDisc :: Double -> Double -> Double
cubicDisc q r = q**3 + r**2


{- Function: cubedRoot ----------------------------------------------

    Description: Function that approximates the cubed root value of
      a double.
    
    Input: A double as x

    Output: The approximate square root of x as a double           -}

cubedRoot :: Double -> Double
cubedRoot x = x***(1/3) -- Using the *** infix function allows proper
                        -- evaluation of cubed roots (-ve values).

                      
{- cubicS -----------------------------------------------------------

    Description: Function that approximates value S from values of R
      and Q.

    Input: Values R and Q as doubles.

    Output: A double value approximately equivalent to the value S -}

cubicS :: Double -> Double -> Double
cubicS q r = cubedRoot ( r + sqrt(cubicDisc q r) )


{- cubicT -----------------------------------------------------------

    Description: Function that approximates value T from values of R
      and Q.

    Input: Values R and Q as doubles.
    
    Output: A double value approximately equivalent to the value T -}
cubicT :: Double -> Double -> Double
cubicT q r = cubedRoot ( r - sqrt(cubicDisc q r) )


{- cubicRealSolutions -----------------------------------------------
    Description: Function to determine the real solutions (if any)
      of a cubic function, given the values of a b c and d.
    
    Input: Values a, b, c, and d as doubles from a cubic equation.

    Output: List of cubic real solutions as doubles                -}

cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
    | isNaN sqrtDisc  = []
    | sqrtDisc === 0  = [
                          s+t - b/(3*a),
                          (s+t)/(-2) - b/(3*a),
                          (s+t)/(-2) - b/(3*a)
                        ]
    | sqrtDisc > 0    = [s+t - b/(3*a)]
    | otherwise       = [] -- should never happen
    where
        sqrtDisc = sqrt (cubicDisc q r)
        s        = cubicS q r
        t        = cubicT q r
        q        = cubicQ a b c
        r        = cubicR a b c d

        (===) :: Double -> Double -> Bool
        x === y = let
          tol = 1e-3
          in abs (x-y) <= tol

    
{- (TO EVAL TESTS) REMOVE OPEN COMMENT

{- General function to check if two float or double values are approximately
   equivalent to each other within a tolerance of 1e-4 -}
(=.) :: (Num a, Fractional a, Ord a) => a -> a -> Bool
x =. y
    | abs(x-y) <= 1.0e-4 = True
    | otherwise = False

-- tests contains various tests, stored as bools. True indicates expected output, false indicates poor output.
tests =  [

    cubicQ 1 1 1 =. 0.22222,            
    cubicQ 2.5 2 1.5 =. 0.12888,
    cubicQ (-3) 1 1 =. (-0.12345),
    
    cubicR 1 1 1 1 =. (-0.37037),
    cubicR (-3.1) 2.1 1.1 0 =. 0.051576,
    cubicR 0.1 (-2.1) 1.1 5 =. 279.5,
    isNaN (cubicR 0 0 0 0),
    
    cubicDisc (-2) 0 =. (-8.0),
    cubicDisc (-4) 8 =. 0.0,
    cubicDisc 2.5 5.0 =. 40.6250,
    
    cubedRoot 5 =. 1.70998,
    cubedRoot (-8) =. (-2),
    cubedRoot 0 =. 0,
    cubedRoot (-9.3) =. (-2.10294),

    {- cubedRoot function works, cubicDisc function works, and since cubedRoot has a domain of all
       real numbers, it can be assumed that cubic S and cubic T are valid and correct functions.
       to confirm this experimentally, cubicRealSolutions will be run. -}

    null (cubicRealSolutions 1 0 (-3) 0),
    head(cubicRealSolutions 1 2 3 4) =. (-1.65063),
    null(cubicRealSolutions 1 15 5 0)
    null(cubicRealSolutions 2 9 13 6)

        ]

-- Run 'ct Tests' in console to quickly verify the preloaded tests.
cT :: [Bool] -> String
cT [] = "No tests"
cT (x:xs)
    | x && not (null xs) = cT xs
    | not x  = "One or more tests produced unexpected output"
    | otherwise = "All tests passed"


REMOVE CLOSE COMMENT -}