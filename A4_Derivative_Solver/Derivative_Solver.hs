module Derivative_Solver where

-- Derivative Solver
-- Name: Nathan Hart
-- Date: Nov 28 2022


{- Datatype: MathExpr -----------------------------------------------

    Description: An Abstract Syntax Tree (AST) for encoding
        mathematical expressions. The MathExpr datatype is a sum type
        with representations for limited common basic algebraic
        expressions.

        One issue with this representation is that multiple,
        algebraically identical expressions can be written as unique
        literals of this datatype. The 'simplify' expression below
        attempts to resolve this issue somewhat.
        
    Example: The expression "(abs (2*X + 1)) ^ 3"
        can be encoded as
        "Power (Abs (Add (Mult (Coef 2) X) (Coef 1))) 3"           -}

data MathExpr a =
    X
    | Coef a
    | Add (MathExpr a) (MathExpr a)
    | Mult (MathExpr a) (MathExpr a)
    | Power (MathExpr a) Int
    -- Uses Int: integer exponentiation (^^) is used when evaluating.
    | Cos (MathExpr a)
    | Sin (MathExpr a)
    | Abs (MathExpr a)

    deriving (Eq,Show,Read)



{- Instance: Num a => Num (MathExpr a) -----------------------------

    Description: Specifications for how basic Num type methods work
        on MathExpr as a instance of the Num class. Act as
        constructors for building math expressions.

    Specifications: Num a 
 
    Method Definitions: (+) :: a -> a -> a
        (*) :: a -> a -> a
        negate :: a -> a
        abs :: a -> a
        fromInteger :: Integer -> a
        *other methods are unimplemented (produce error)

    Output: Num (MathExpr a)                                       -}

instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = case x of -- Perform minor simplification   
                    Coef a -> Coef (-a)
                    _ -> Mult x (Coef (-1))
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left unimplemented"


{- Instance: Fractional a => Fractional (MathExpr a) ----------------

    Description: Specifications for how basic Fractional type methods
        work on MathExpr as a instance of the Fractional class. Act
        as constructors for building math expressions.

    Specifications: Fractional a
 
    Method Definitions: recip :: a -> a
        fromRational :: a -> a
    
    Output: Fractional (MathExpr a)                                -}

instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)


{- Instance: Floating a => Floating (MathExpr a) --------------------
    Description: Specifications for how basic Floating type methods
        work on MathExpr as a instance of the Floating class. Act as
        constructors for building math expressions.
    
    Specifications: Floating a

    Method Definitions: pi :: a
        sin :: a -> a
        cos :: a -> a
        *other methods are unimplemented (produce error)

    Output: Floating (MathExpr a)                                  -}

instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Sin
  cos     = Cos
  log     = error "log is left unimplemented"
  asin _  = error "asin is left unimplemented"
  acos _  = error "acos is left unimplemented"
  atan _  = error "atan is left unimplemented"
  sinh _  = error "sinh is left unimplemented"
  cosh _  = error "cosh is left unimplemented"
  tanh _  = error "tanh is left unimplemented"
  asinh _ = error "asinh is left unimplemented"
  acosh _ = error "acosh is left unimplemented"
  atanh _ = error "atanh is left unimplemented"
  exp _   = error "exp is left unimplemented"
  sqrt _  = error "sqrt is left unimplemented"


{- Function: eval ---------------------------------------------------

    Description: eval takes in a numerical MathExpr and a numerical
        value for X and computes the value of the MathExpr as a
        number at the given value for x. 

    Specifications: Floating a, Eq a 
 
    Input: e :: MathExpr a, Math expression to evaluate
        v :: a, given numerical value for x

    Output: a, value of MathExpr evaluated at v                    -}

eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval e v = case e of    
    X -> v 
    Coef x -> x
    Add x y -> expr2 x (+) y
    Mult x y -> expr2 x (*) y
    Power x i -> eval x v ^^ i
    Cos x -> expr1 cos x
    Sin x -> expr1 sin x
    Abs x -> expr1 abs x
    where expr1 = \op x -> op (eval x v)
          expr2 = \x op y -> eval x v `op` eval y v


{- Function: diff ---------------------------------------------------
    
    Description: diff takes in a numerical MathExpr and returns a new
        MathExpr which is the symbolic derivative of the input
        expression.

    Specifications: Floating a, Eq a 
 
    Input: e :: MathExpr a, Math expression to differentiate

    Output: MathExpr a, symbollically differentiated input         -}

diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff e = let

    diff' e' = case e' of
        X -> 1
        Coef _ -> 0
        Add x y -> diff x + diff y
        Mult x y -> diff x * y + x * diff y
        Power x i -> n * Power x (i-1) * diff x
            where n = fromIntegral i
        Cos x -> -sin x * diff x
        Sin x -> cos x * diff x
        Abs x -> x / abs x * diff x

    in simplify $ diff' e


{- Function: pretty -------------------------------------------------
    
    Description: pretty takes a MathExpr and converts it to a string
        format using conventional algebraic symbols. 

    Specifications: Show a, Num a
 
    Input: e :: MathExpr a, Math expression to convert to string

    Output: String, string representation of input expression.     -}

pretty :: (Show a, Num a) => MathExpr a -> String
pretty e = let 

        bracket a = "(" ++ a ++ ")"
        ins op a b = bracket $ pretty a ++ op ++ pretty b
        pre op a = op ++ bracket (pretty a)

        pretty' e' = case e' of

            X -> "X"
            Coef a -> bracket $ show a
            Add a b -> ins "+" a b
            Mult a b -> ins "*" a b
            Power a i -> ins "^^" a $ fromIntegral i
            Cos a -> pre "cos" a
            Sin a -> pre "sin" a
            Abs a -> pre "abs" a
            
    in pretty' e


{- Function: simplify -----------------------------------------------

    Description: Function to perform minor simplication of values of
        MathExpr to symbolically equivalent values.

    Note: Will only perform minor simplication, including
        simpliciation of identity elements (e.g. val + 0 -> val).
        Will also evaluate the simple computations such as adding two
        coefficients in the same Add or Mult construct. Finally, it
        will recursively simplify sub expressions, and attempt to
        simplify the resultant construction. Recursion will terminate
        when no further simplication can occur.

    Specifications: Num a, Eq a
 
    Input: e :: MathExpr a, Math expression to simplify

    Output: MathExpr a, simplified expression                      -}

simplify :: (Num a, Eq a) => MathExpr a -> MathExpr a
simplify e = let

        simpleAdd a@(Add x y) = let 

            a' = Add x' y'
            x' = simplify x
            y' = simplify y

            in case a of
                Add 0 y -> y'
                Add x 0 -> x'
                Add (Coef a) (Coef b) -> Coef (a + b)
                Add x y | x == y -> simplify $ Mult x' 2
                _ -> if a' == a then a else simplify a'

        simpleMult m@(Mult x y) = let

            m' = Mult x' y'
            x' = simplify x
            y' = simplify y

            in case m of
                Mult 0 _ -> 0
                Mult _ 0 -> 0
                Mult 1 y -> y'
                Mult x 1 -> x'
                Mult (Coef a) (Coef b) -> Coef (a * b)
                Mult x y | x == y -> Power x' 2
                _ -> if m' == m then m else simplify m'

        simplePower p = case p of
            Power _ 0 -> Coef 1
            Power x 1 -> simplify x
            Power x i -> Power (simplify x) i

    in case e of
        X -> X 
        Coef x -> Coef x
        Add _ _ -> simpleAdd e
        Mult _ _ -> simpleMult e
        Power _ _ -> simplePower e
        Cos x -> Cos (simplify x)
        Sin x -> Sin (simplify x)
        Abs x -> Abs (simplify x)

{-
Questions:
    How to implement simplify so it occurs at the moment of construction?
    How to get pretty to operate differently for general types than for Num type?
        -> Want to include simplication before prettying if possible
        -> General question, how to define functions to operate differently on different
           datatypes?

TODO:
    - Implement Pretty in terms of unique classes for both num types and non-num types
-}