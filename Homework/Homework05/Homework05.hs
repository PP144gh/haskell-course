-- Create a higher-order function that takes 3 parameters: A function and the two parameters that that function takes, and
-- flips the order of the parameters.
-- For example this: `(/) 6 2` returns `3`. But this: `flip' (/) 6 2` returns `0.3333333333`

flip2 :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip2 f a b = f b a


-- Create the `uncurry'` function that converts a curried function to a function on pairs. So this: `(+) 1 2` that returns `3` can be written as
-- `uncurry' (+) (1,2)` (with the two different arguments inside a pair).


f2 :: Int -> Int -> Int
f2 = \x -> \y ->  x + y
f3 :: Int
f3 = f2 1 2

uncurry2 :: (a -> b -> c) -> ((a,b) -> c)
uncurry2 (#) (x,y) = x # y
-- for operators or

uncurry3 :: (a -> b -> c) -> ((a,b) -> c)
uncurry3 f (x,y) = f x y

-- uncurry3 is a higher order function because it receives a function f as argument. the function f itself is a curried function, because it is a function that returns a function.

-- uncurry3 f2 (1,2) same as uncurry3 (+) (1,2)

-- Create the `curry'` function that converts an uncurried function to a curried function. So this: `fst (1,2)` that returns `1` can be written as
-- `curry' fst 1 2` (with the tuple converted into two different arguments).


curry2 :: ((a,b) -> c) -> a -> b -> c
curry2 g x y = g (x,y)


-- Use higher-order functions, partial application, and point-free style to create a function that checks if a word has an uppercase letter.
-- Start with using just higher-order functions and build from there. 


check :: String -> Bool
check = any (`elem` ['A'..'Z'])

--any is a higher order function, function elem is used in partial application (see any (>4) [1,2,3,4]) and it is written in point-free style (not check word = any (`elem` ['A'..'Z']) word).
-- Chars are ordered lexicographically so a list A..Z can be created like a list 1..10.

-- Create the `count` function that takes a team ("Red", "Blue", or "Green") and returns the amount of votes the team has inside `votes`.

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: String -> [String] -> Int
count team  = length . filter (== team) 

-- this works for every list [String] of votes
--or
count' :: String -> Int
count' x = length . filter (== x) $ votes

-- this works only for the list votes defined above. it is not an argument. it is built in, in the function.

-- Create a one-line function that filters `cars` by brand and then checks if there are any left.

cars :: [(String,Int)]
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]


checkCar:: String -> Bool
checkCar brand = any (\(_,b) -> b>0) . filter (\(a,_) -> a == brand) $ cars









--- NOTES ---


--higher order fuction : fuction which receives function as argumet ex:

fsquared :: (t -> t) -> t -> t
fsquared f x = f (f x)

-- different from


complexFunc1 :: Int -> Int
complexFunc1 x = x + 1

func1 :: Int -> Int
func1 x = complexFunc1 (complexFunc1 x)


-- fsquared applies a function twice, any function. func1 applies the function complexFunc1 twice.
-- that is why the type signatures are different.

func1' :: Int -> Int
func1' x = fsquared complexFunc1 x


--examples
-- filter function
-- selects all elements which make the predicate true

fruitWithA = filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
                where tempFunct x = 'a' `elem` x

-- Output: ["Banana","Pear","Grape"] 

-- any function

---- Only for lists:  any :: (a -> Bool) -> [a] -> Bool
--This function also takes a predicate and a list of elements. 
--But this one checks if there exists **any** element in the list for which the predicate holds.

--For example, here we're checking if any of the elements of the list is greater than 4. If only one is, `any` returns `True`, else, it returns `False`:
biggerThan4 :: (Ord a, Num a) => a -> Bool
biggerThan4 x = x > 4

-- any biggerThan4 [1,2,3,4,5] output: True

cars2 = [("Toyota",0), ("Nissan",3), ("Ford",1)]

biggerThan0 (_,x) = x > 0

-- any biggerThan0 cars2 output: True

-- Lambda functions. anonymous function. useful for funcs that are only used once.

-- \x y -> x * y  (\ means lambda)


-- instead of biggerThan4 x = x > 4

-- for any biggerThan4 [1,2,3,4] 


-- do 
--any (\x -> x > 4) [1,2,3,4]

-- and
-- filter (\x -> 'a' `elem` x) ["Apple", "Banana", "Pear", "Grape", "Wood"]


--  precedence an associativity
-- :i (+) output: infixl 6
-- this is a fixity declaration. 6 is the precedence, (higher precedence means higher priority)
-- and infixl is related to the associativity.
-- When two operators have the same precedence, the associativity tells you which side (left with `infixl` or right with `infixr`) will be evaluated first.
-- infix means no associativty (case of ==), you need to specify parentheses.

-- ex: 
-- 1 + 2 + 3 + 4  -- infixl: Same as ((1 + 2) + 3) + 4

--1 : 2 : 3 : [] -- infixr: Same as 1 : (2 : (3 : []))

--True == (False == False) -- infix: If you remove parenthesis, you'll get an error.

-- evaluation order can be changed with parentheses:
-- :i (**) -- infixr 8 **

-- 2**3**4  -- infixr: Same as 2 ** (3 ** 4)
-- different from (2**3)**4

-- one can define the precedence and associativity of a operator when creating it:
-- x +++ y = x + y -- Creating +++ operator
--infixl 7 +++    -- Setting fixity of operator

-- 1 +++ 2 * 3  -- 9
--The result is 9 because `+++` and `*` are both left-associative and have the same precedence.

-- note: Operators without an explicit fixity declaration are infixl 9.
-- Function application the "whitespace operator" always has the highest precedence (imagine precedence 10).


-- curried functions: functions that return functions (all functions can be thought like that)

-- take func

--add3 :: Int -> Int -> Int -> Int
-- add3 x y z = x + y + z
--same as 
-- add3 :: Int -> (Int -> (Int -> Int))
--add3 = \x -> (\y -> (\z -> x + y + z))


-- because `->` is right-associative, we can remove the use-less parentheses of both the signature and definition to get a cleaner code:


-- add3 :: Int -> Int -> Int -> Int
-- add3 = \x -> \y -> \z -> x + y + z


-- useful for PARTIAL APPLICATION


-- Partial application in Haskell means that you provide fewer arguments than the maximum amount the function accepts.




-- createEmail :: String -> String -> String -> String
--createEmail domain name lastName = name ++ "." ++ lastName ++ "@" ++ domain


{-


createEmailTeckel :: String -> String -> String
createEmailTeckel = createEmail "teckel-owners.com"

createEmailSCL :: String -> String -> String
createEmailSCL = createEmail "secret-cardano-lovers.com"

createEmailTeckel "Robertino" "Martinez"
createEmailSCL "Vitalik" "Buterin"

-}


-- createEmailTeckel and createEmailSCL are functions that only take 2 arguments name and lastName.


-- order of arguments matters!!!
-- If, for some reason, the parameter you want to apply is not the first one and you are not allowed to rewrite the existing function, you can create a helper function:


{-
-- With partial application:

createEmailJohn :: String -> String -> String
createEmailJohn lastName domain = createEmail domain "John" lastName

-- Without partial application:

createEmail' :: String -> String -> String -> String
createEmail' name lastName domain = createEmail domain name lastName



-}

-- SECTIONS
-- The partial application of an infix operator is called a *section*.

{-
(++ "ing") "Think"     -- Same as \x -> x ++ "ing"
output: "Thinking"

("Anti" ++) "library"  -- Same as \x -> "Anti" ++ x
output: Antilibrary
-}


-- Function application operator $

{-
($) :: (a -> b) -> a -> b
f $ x =  f x

-}
-- redundant def?
-- The "white space" operator has the highest left-associative precedence.
-- The function application operator (`$`) has the lowest right-associative precedence: `infixr 0 $`.

-- ex: 
--f g h x      = ((f g) h) x

--f $ g $ h x  =  f (g (h x))


 {-

 (2 *) 3 + 4    -- Same as: ((2 *) 3) + 4 -- 10
(2 *) $ 3 + 4  -- Same as: (2 *) (3 + 4) -- 14

max 5 4 + 2    -- Same as: ((max 5) 4) + 2 -- 7
max 5 $ 4 + 2  -- Same as: (max 5) (4 + 2) -- 6
 
 -}


{-
As you can see in the previous examples, when using `$`, the whole expression on its right is applied as the parameter to the function on its left. So you can see how using `$` is like surrounding everything to its right between parentheses.

This brings us to the primary use of `$`: Omitting parentheses!

In the following expression, there are 3 opportunities to remove parenthesis, so we remove them:
-}

-- espaço significa faz o que está à esquerda primeiro. $ significa faz o que está a direita primeiro.
-- para um só argumento isto é a mesma coisa.


{-
-- All these expressions are equivalent:

show ((2**) (max 3 (2 + 2)))

show $ (2**) (max 3 (2 + 2))

show $ (2**) $ max 3 (2 + 2)

show $ (2**) $ max 3 $ 2 + 2
-}


-- Function Composition

-- 3 compositions in a row
--complicatedF :: [Int] -> Bool
--complicatedF x = any even (filter (>25) (tail ( take 10 x)))

-- This can be done by abstracting function composition to an operator. And because, in mathematics, the composition symbol is a ring that kind of resembles a dot, we'll use a dot:
-- (.)  :: (b -> c) -> (a -> b) -> a -> c
--f . g = \x -> f (g x)
--infixr 9 .

-- hence 

{-
complicatedF :: [Int] -> Bool
complicatedF x = any even . filter (>25) . tail . take 10 $ x

-}

--  every function to both sides of the `.` operator takes a single argument or is partially applied until it takes a single argument.


{-
show ((2**) (max 3 (2 + 2)))
same as 
show . (2**) . max 3 $ 2 + 2

-}

-- Point-free style (tacit programming)
-- instead of

{-
fourOrLarger :: Int -> Int
fourOrLarger x = max 4 x

add1 :: Int -> Int
add1 x = 1 + x-}

-- one can do


{-
fourOrLarger :: Int -> Int
fourOrLarger = max 4

add1 :: Int -> Int
add1 = (1+)

also previous function can be rewritten as

complicatedF :: [Int] -> Bool
complicatedF = any even . filter (>25) . tail . take 10
-}


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 


print((/) 6 2)
print(flip2 (/) 6 2)
print(filter even [1..20])
print(fruitWithA)
print(any biggerThan4 [1,2,3,4,5])

-- print(f)
print(f3)

print (check "Ola")
print(count "Red" votes)
print(checkCar "Nissan")