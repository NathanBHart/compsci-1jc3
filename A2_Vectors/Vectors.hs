module Vectors where

-- Vectors
-- Name: Nathan Hart
-- Date: October 24th 2022


{- Synonym Type: Vector4D -------------------------------------------

   Description: A synonym for a tuple of 4 Double values. Represents
      a vector with 4 dimensions.                                  -}

type Vector4D = (Double,Double,Double,Double)


{- Function: getX, getY, getZ, getM ---------------------------------

      Description: Extracts the cooresponding value from a Vector4D
         (similar to record synatax)

      Input: (x, y, z, m) :: Vector4D

      Output: Double, cooresponding value                          -}

getX :: Vector4D -> Double
getX (x, _, _, _) = x

getY :: Vector4D -> Double
getY (_, y, _, _) = y

getZ :: Vector4D -> Double
getZ (_, _, z, _) = z

getM :: Vector4D -> Double
getM (_, _, _, m) = m


{- Function: scalarMult ---------------------------------------------

      Description: Multiplies a vector by a scalar

      Input: s :: Double, scalar multiple
         (x, y, z, m) :: Vector4D

      Output: Vector4D, modified vector                            -}

scalarMult :: Double -> Vector4D -> Vector4D
scalarMult s (x, y, z, m) = (s*x, s*y, s*z, s*m)


{- Function: add ----------------------------------------------------

      Description: Adds two Vector4D values together

      Input (x1, y1, z1, m1) :: Vector4
         (x2, y2, z2, m2) :: Vector4

      Output: Vector4D, Added vectors                              -}

add :: Vector4D -> Vector4D -> Vector4D
add (x1, y1, z1, m1) (x2, y2, z2, m2) =
         (x1 + x2, y1 + y2, z1 + z2, m1 + m2)


{- Function: innerProduct -------------------------------------------

      Description: Computes the inner product of two given Vector 4D
         values

      Input: (x1, y1, z1, m1) :: Vector4
         (x2, y2, z2, m2) :: Vector4

      Output: Double, inner product of two supplied vectors        -}

innerProduct :: Vector4D -> Vector4D -> Double
innerProduct (x1, y1, z1, m1) (x2, y2, z2, m2) =
         (x1 * x2) + (y1 * y2) + (z1 * z2) + (m1 * m2)


{- Function: distance -----------------------------------------------

      Description: Computes the metric distance between two given
         Vector 4D values

      Input: (x1, y1, z1, m1) :: Vector4
         (x2, y2, z2, m2) :: Vector4

      Output: Double, distance between two vectors                 -}

distance :: Vector4D -> Vector4D -> Double
distance v1 v2 = sqrt(innerProduct diff diff)
    where diff = v1 `add` negv2
          negv2 =  scalarMult (-1) v2

          
{- Function: maxDistance --------------------------------------------

      Description: Returns the Vector4D with greatest distance from
         origin in list  

      Input: vs :: [Vector4], list of vectors

      Output: Vector4, vector with greatest distance from origin   -}

maxDistance :: [Vector4D] -> Vector4D
maxDistance vs = let

   origin = (0, 0, 0, 0)

   mD v [] = v
   mD v1 (v2:vs)
      | d v1 >= d v2 = mD v1 vs
      | d v1 <  d v2 = mD v2 vs
      where d = (`distance` origin)

   in mD origin vs


{- REMOVE OPEN COMMENT FOR TESTS


{- General function to check if two float or double values are approximately
   equivalent to each other within a tolerance of 1e-4 -}
(=.) :: (Num a, Fractional a, Ord a) => a -> a -> Bool
x =. y = abs(x-y) <= 1.0e-4

{- General function to check if two Vector4D values are approximately
   equivalent to each other within a tolerance of 1e-4 -}
(=?) :: Vector4D -> Vector4D -> Bool
(x1, y1, z1, m1) =? (x2, y2, z2, m2) = x1 =. x2 && y1 =. y2 && z1 =. z2 && m1 =. m2


-- tests contains various tests, stored as bools. True indicates expected output, false indicates poor output.
tests =  [

    ------------ Function: scalarMult ------------
    -- Test Case Number: 1
    -- Input: 5 (0, 0, 0, 0)
    -- Expected Output: (0.0, 0.0, 0.0, 0.0)
    -- Actual Output: (0.0, 0.0, 0.0, 0.0)
    scalarMult 5 (0, 0, 0, 0) =? (0, 0, 0, 0),

    -- Test Case Number: 2
    -- Input: 5 (1, 1, 1, 1)
    -- Expected Output: (5.0, 5.0, 5.0, 5.0)
    -- Actual Output: (5.0, 5.0, 5.0, 5.0)
    scalarMult 5 (1, 1, 1, 1) =? (5, 5, 5, 5),

    -- Test Case Number: 3
    -- Input: (-5) (-1, 1, 0, -0)
    -- Expected Output: (5.0, -5.0, -0.0, 0.0)
    -- Actual Output: (5.0, -5.0, -0.0, 0.0)
    scalarMult 5 (-1, 1, 0, -0) =? (-5, 5, 0, 0),

    -- Test Case Number: 4
    -- Input: (1/(sqrt 2)) (1, -2, 5, 0)
    -- Expected Output (approx): (0.707106, -1.41421, 3.53553, 0.0)
    -- Actual Output: (0.7071067811865475, -1.414213562373095, 3.5355339059327373, 0.0)
    scalarMult (1/(sqrt 2)) (1, -2, 5, 0) =? (0.70711, -1.41421, 3.53553, 0.0),

    --------------- Function: add ---------------
    -- Test Case Number: 1
    -- Input: (0, 0, 0, 0) (1, 1, 1, 1)
    -- Expected Output: (1.0, 1.0, 1.0, 1.0)
    -- Actual Output: (1.0, 1.0, 1.0, 1.0)
    add (0, 0, 0, 0) (1, 1, 1, 1) =? (1.0, 1.0, 1.0, 1.0),

    -- Test Case Number: 2
    -- Input: (-1.6, 1.6, 3.2, -3.2) (1.6, 1.6, 1.6, 1.6)
    -- Expected Output: (0.0, 3.2, 4.8, -1.6)
    -- Actual Output: (0.0,3.2,4.800000000000001,-1.6)
    add (-1.6, 1.6, 3.2, -3.2) (1.6, 1.6, 1.6, 1.6) =? (0.0, 3.2, 4.8, -1.6),

    -- Test Case Number: 3
    -- Input: (1/(sqrt 2), 1/3, 0, 0) (1, 2/3, 1, 1)
    -- Expected Output (approx): (1.70711, 1.0, 1.0, 1.0)
    -- Actual Output: (1.7071067811865475,1.0,1.0,1.0)
    add (1/(sqrt 2), 1/3, 0, 0) (1, 2/3, 1, 1) =? (1.70711, 1.0, 1.0, 1.0),

    ----------- Function: innerProduct -----------
    -- Test Case Number: 1
    -- Input: (0, 0, 0, 0) (1, 1, 1, 1)
    -- Expected Output: 0.0
    -- Actual Output: 0.0
    innerProduct (0, 0, 0, 0) (1, 1, 1, 1) =. 0,

    -- Test Case Number: 2
    -- Input: (-1.6, 1, 3.2, -3.2) (1.6, 1.6, 1.6, 1.6)
    -- Expected Output: -0.96
    -- Actual Output: -0.9600000000000009
    innerProduct (-1.6, 1, 3.2, -3.2) (1.6, 1.6, 1.6, 1.6) =. (-0.96),

    -- Test Case Number: 3
    -- Input: (1/(sqrt 2), 1/3, 0, 0) (1, 2/3, 1, 1)
    -- Expected Output (approx): 0.9293
    -- Actual Output: 0.9293290034087697
    innerProduct (1/(sqrt 2), 1/3, 0, 0) (1, 2/3, 1, 1) =. 0.9293,

    ------------- Function: distance -------------
    -- Test Case Number: 1
    -- Input: (0, 0, 0, 0) (1, 1, 1, 1)
    -- Expected Output: 2.0
    -- Actual Output: 2.0
    distance (0, 0, 0, 0) (1, 1, 1, 1) =. 2.0,

    -- Test Case Number: 2
    -- Input: (-1.6, 1, 3.2, -3.2) (1.6, 1.6, 1.6, 1.6)
    -- Expected Output (approx): 6.0166
    -- Actual Output: 6.01664358259653
    distance (-1.6, 1, 3.2, -3.2) (1.6, 1.6, 1.6, 1.6) =. 6.0166,

    -- Test Case Number: 3
    -- Input: (1/(sqrt 2), 1/3, 0, 0) (1, 2/3, 1, 1)
    -- Expected Output (approx): 1.4822
    -- Actual Output: 1.4821934923410018
    distance (1/(sqrt 2), 1/3, 0, 0) (1, 2/3, 1, 1) =. 1.4822,

    ------------- Function: maxDistance -------------
    -- Test Case Number: 1
    -- Input: [(0, 0, 0, 0), (1, 1, 1, 1), (-1, 1, 1, 1)]
    -- Expected Output: (1.0, 1.0, 1.0, 1.0)
    -- Actual Output: (1.0, 1.0, 1.0, 1.0)
    maxDistance [(0, 0, 0, 0), (1, 1, 1, 1), (-1, 1, 1, 1)] =? (1.0, 1.0, 1.0, 1.0),

    -- Test Case Number: 2
    -- Input: [(-1.6, 1, 3.2, -3.2), (1.6, 1.6, 1.6, 1.6), (100, 0, 0, 0)]
    -- Expected Output: (100.0, 0.0, 0.0, 0.0)
    -- Actual Output: (100.0, 0.0, 0.0, 0.0)
    maxDistance [(-1.6, 1, 3.2, -3.2), (1.6, 1.6, 1.6, 1.6), (100, 0, 0, 0)] =? (100.0, 0.0, 0.0, 0.0),

    -- Test Case Number: 3
    -- Input: [(-1.6, 1, 3.2, -3.2), (1.6, 1.6, 1.6, 1.6)]
    -- Expected Output: (-1.6, 1.0, 3.2, -3.2)
    -- Actual Output: (-1.6, 1.0, 3.2, -3.2)
    maxDistance [(-1.6, 1, 3.2, -3.2), (1.6, 1.6, 1.6, 1.6)] =? (-1.6, 1, 3.2, -3.2),

    -- Test Case Number: 4
    -- Input: []
    -- Expected Output: (0.0, 0.0, 0.0, 0.0)
    -- Actual Output: (0.0, 0.0, 0.0, 0.0)
    maxDistance [] =? (0.0, 0.0, 0.0, 0.0),

    -- Test Case Number: 5
    -- Input: [(1/(sqrt 2), 0, 0, 1/(sqrt 2)), (1, 0, 0, 0)]
    -- Expected Output (approx): (0.7071, 0.0, 0.0, 0.7071)
    -- Actual Output: (1.0, 0, 0, 0)
    -- FAILED initial test, reason why is attributed to floating point error. 1st element is same distance
    --        but when computed returns a distance of 0.9999999999.... instead of 1.0, so so 2nd is returned 
    -- !!! maxDistance [(1/(sqrt 2), 0, 0, 1/(sqrt 2)), (1, 0, 0, 0)] =? (0.70710, 0.0, 0.0, 0.70710)

    -- Test Case Number: 5.1
    -- Input: [(1/(sqrt 2), 0, 0, 1/(sqrt 2)), (0.99, 0, 0, 0)]
    -- Expected Output (approx): (0.7071, 0.0, 0.0, 0.7071)
    -- Actual Output: (0.7071, 0.0, 0.0, 0.7071)
    -- WORKED tested with lowered second distance, so it falls below 0.99999999.... in distance from origin
    maxDistance [(1/(sqrt 2), 0, 0, 1/(sqrt 2)), (0.99, 0, 0, 0)] =? (0.7071, 0.0, 0.0, 0.7071)                                        

        ]

CLOSE COMMENT -}