-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?

{-
The Bounded class is used to name the upper and lower limits of a type. 

The Bounded class may be derived for any enumeration type; minBound is the first constructor listed in the data declaration and maxBound is the last.

example:


instance Bounded Boost where
    minBound = Boost 1
    maxBound = Boost 10

-}
-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.


{-
maxBound :: Int in ghci gives 9223372036854775807
minBound :: Int in ghci gives -9223372036854775807


maxBound :: Int in ghci gives 18446744073709551615
minBound :: Int in ghci gives 0


word is a unsigned int type. maxbound is 2x the int one.

-}

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?


{-

class  Enum a  where  
    succ, pred     :: a -> a  
    toEnum         :: Int -> a  
    fromEnum       :: a -> Int  
    enumFrom       :: a -> [a]            -- [n..]  
    enumFromThen   :: a -> a -> [a]       -- [n,n'..]  
    enumFromTo     :: a -> a -> [a]       -- [n..m]  
    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]  
 
    -- Default declarations given in Prelude
Class Enum defines operations on sequentially ordered types. 
The functions succ and pred return the successor and predecessor, respectively, of a value. 
The functions fromEnum and toEnum map values from a type in Enum to and from Int. The enumFrom... methods are used when translating arithmetic sequences.

-- This type class is the one that allows us to create ranges of values like [3..] and ['a'..'h'] using the functions enumFrom etc.


-}

-- Question 4
-- Add type signatures to the functions below and use type variables and type classes.
-- Then uncomment the functions and try to compile.

import Data.Char ( isDigit )


f1 :: (Show a, Fractional a) => a -> a -> [Char] -> [Char]
f1 x y z = show (x/y) ++ z

--show requires Show and / requires fractional


f2 :: (Show a, Fractional a, Read a, Enum a) =>[Char] -> a -> a -> String
f2 x y z = if all isDigit x
           then show $ foldl (/) (read x) [y..z]
           else "Error: can not parse 1st parameter."

--show requires Show, / requires fractional, read requires Read, [y..z] requires Enum

f3 :: (Eq p, Bounded p, Enum p) => p -> p
f3 x = if x == maxBound
       then minBound
       else succ x

--this function gives the next element of the sequentially ordered type (Int, for example). if the current elemnt is the maxBound of the type, it gives the minBound.

-- == requires Eq, maxBound and minBound require Bounded, succ requires Enum


-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.


{-

Integral type class contains Int and Integer.

fromInteger: fromInteger :: Num a => Integer -> a
Will convert any Integer into any Num .

fromIntegral:  fromIntegral :: (Num b, Integral a) => a -> b
Will convert from any Integral type (Int, Integer) into any Numeric type (which includes Int, Integer, Rational, and Double).

toInteger: toInteger:: Integral a => a -> Integer
Will convert (Int,Integer) to Integer


RealFrac type class contains Rational (fractions of whole integers, i.e., rational numbers) and Double (includes irrational numbers).
Real type class includes Integral and RealFrac.
Fractional type class includes numbers that are closed under / operation (Float and Double).

realtoFrac: realToFrac:: (Real a, Fractional b) => a -> b


fromRational: fromRational :: Fractional a => Rational -> a


toRational: toRational :: Real a => a -> Rational

-- Converting from real-fractional numbers to integral numbers

(they involve some kind of approximation of the realfrac number, when it is not whole.)

ceiling  :: (RealFrac a, Integral b) => a -> b
ceiling 2.3 = 3

floor    :: (RealFrac a, Integral b) => a -> b
floor 2.3 = 2

truncate :: (RealFrac a, Integral b) => a -> b
truncate 2.3 = 2 , truncate 2.8 = 2

round    :: (RealFrac a, Integral b) => a -> b
round 2.3 = 2, round 2.8 = 3

--Converting between different floating-point precisions

float2Double :: Float -> Double
double2Float :: Double -> Float



more info in: https://wiki.haskell.org/Converting_numbers
-}



--- Notes --

--defining a type as an instance of a type class gives it more functionally while still being safe and controllable.


-- :i Bool in ghc gives


{-

type Bool :: *
data Bool = False | True
    -- Defined in ‘GHC.Types’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Show Bool -- Defined in ‘GHC.Show’
instance Read Bool -- Defined in ‘GHC.Read’
instance Bounded Bool -- Defined in ‘GHC.Enum’

instances inform what type classes Bool is part of.



pag 31 of book fore more common type classes.
-}


main :: IO ()
main = do

print(f3 9223372036854775807 :: Int)