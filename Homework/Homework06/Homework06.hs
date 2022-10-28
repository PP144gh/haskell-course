
-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite list with
-- the value provided as every element of the list.
--
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.List (foldl')
import DynFlags (supportedLanguagesAndExtensions)
{-# HLINT ignore "Use foldr" #-}

repeat' :: Enum a => a -> [a]
repeat' x = x:repeat' x
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson (comes with Haskell),
-- create a function called `replicate'` that takes a number `n` and a value `x` and creates a list
-- of length `n` with `x` as the value of every element. (`n` has to be Integer.)
--

replicate' :: Enum a => Int -> a -> [a]
replicate' n x = take' n $ repeat' x

-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]


-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.
--

concat' :: [[a]] -> [a]
concat' [] = []
concat' (l:lists) = l ++ concat' lists
--concat' lists = foldr (++) [] lists

-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]


-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:

zip' :: [a1] -> [a2] -> [(a1, a2)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys


-- >>> zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
--
-- >>> zip' [1] ['a', 'b']
-- [(1,'a')]
--
-- >>> zip' [1, 2] ['a']
-- [(1,'a')]
--
-- >>> zip' [] [1..]
-- []
--
-- >>> zip' [1..] []
-- []



-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys



-- Question 6
-- Write a function called `takeWhile'` that takes a predicate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
--
-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x: takeWhile' p xs
    | otherwise = []

-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.

-- try 1

{-
func :: Int -> String
func n = reverse (printer n) ++ " = " ++ show (factorial n)
    where 
      printer 0 = []
      printer 1 = "1"
      printer d = show d ++ "*" ++ printer (d-1)
      factorial n = product [n,n-1..1]
-}
--try 2 after seeing prof solution. avoiding reverse is elegant.

func :: Int -> String
func n = printer 1 n ++ " = " ++ show (factorial n)
    where 
      printer i f = if i == f then show f else show i ++ "*" ++ printer (i+1) f
      factorial num = product [num,num-1..1]




--prof solution

{-
factorial :: Int -> String
factorial n = accumulate 2 "1" ++ " = " ++ show result
  where
    accumulate x string
      | x > n = string
      | otherwise = accumulate (x + 1) (string ++ "*" ++ show x)
    result = product [1 .. n]


-}


-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery. Assume that the two lists have the beers in the same order.

bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Double)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50

beercost :: [(String, Double)] -> Double
--beercost orderlist = foldl' (+) deliveryCost $ zipWith' (\(_,price) (_,qty)  -> qty * price) orderlist bevogBeerPrices
beercost = foldl' (+) deliveryCost . zipWith' (\(_,price) (_,qty)  -> qty * price) bevogBeerPrices -- with partial application

-- prof solutions

{-

beerCosts :: [(String, Double)] -> Double
beerCosts =
  foldr (+) deliveryCost
    . zipWith (*) (map snd bevogBeerPrices)
    . map snd
    . filter (\name -> fst name `elem` map fst bevogBeerPrices)


-- my lambda function does the same as his composition of functions after the foldr. it starts by filtering bevogBeerPrices to only contain the name fields which are in the order.
-- then takes the quantity and mutiplies by the price, yielding a list of prices, ordered by the name of the beers.

-- in my solution I avoid the initial filtering, since zipWith' already has this built in, it will only compute a number of times equal to the length of the smaller list.
-- the map uses are equivalent to my lambda function definition. the rest is equivalent.

-}


--- NOTES

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

{-

Steps to create your own recursive function

1) Write down the type: This will help you define de function later. (You should always define the type first, even if you're not defining a recursive function.)

2)Enumerate the possible cases you could have based on its inputs. (Sart with the "standard" ones and change or refine them if needed.)

3) Between all the previously instantiated cases, identify which are the simplest ones and define them. (These often are the base (or edge) cases.)

4) Think about what you have available (parameters, functions, operators, other values, operators for that type, etc.).

5) Define the rest of the cases.

6) Reflect on the function. Can the definition be simplified? Can the signature be generalized? (we'll see how to do it in future lessons) Does it do what you intended?

You don't always have to go through these steps. Once you feel more comfortable, you can skip a few or even write the function right off the bat.

In general, the base (or edge) case is usually the "identity" case. A case that doesn't modify the result but just stops the recursion. Here we have a few examples:

Two common standard patterns:

For recursive functions that take non-negative numbers as input, you usually (not always) have a base (or edge) case of 0 or 1 (depending on the operation) and a recursive case of n.
For recursive functions that take lists as input, you usually (not always) have a base (or edge) case of [] (empty list) and a recursive case of (x:xs) (non-empty list).

-}

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
--reverse' (x:xs) = reverse' xs ++ x : []

drop' :: Int -> [a] -> [a]
drop' _ []           = []
drop' n xs | n <= 0  = xs
drop' n (_:xs)       = drop' (n - 1) xs
-- takes first n elements of list and produces a list with the rest. note pattern matching def with conditional n, didnt know this was possible (lol)


take' :: Int -> [a] -> [a]
take' n _      | n <= 0 = []
take' _ []              = []
take' n (x:xs)          = x : take' (n-1) xs


map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
-- returns list only with elements that satisfy p x. p is a predicate, a function that receives an argument of type a and returns Bool.



--extracting foldr pattern
--for instance 
{-
sum' :: [Int] -> Int
sum' [] = 0 -- base value
sum' (x:xs) = (+) x (sum' xs) -- function, here (+), list (x:xs)
-}
-- primitive recursion pattern

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] =  v -- base value
foldr' f v (x:xs) = f x (foldr' f v xs)  --function combining value and recursion
-- f is a function that receives type a and b and returns b

{- EXAMPLE: type a and b are the same here
-- same as: sum [1,2,3,4]
foldr (+) 0 [1,2,3,4] = (+) 1 (foldr (+) 0 [2,3,4])
                      = (+) 1 ((+) 2 (foldr (+) 0 [3,4]))
                      = (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [4])))
                      = (+) 1 ((+) 2 ((+) 3 ((+) 4 (foldr (+) 0 []))))
                      = (+) 1 ((+) 2 ((+) 3 ((+) 4 0))) -- 1 + ( 2 + ( 3 + ( 4 + 0 )))
                      = (+) 1 ((+) 2 ((+) 3 4)) -- 1 + ( 2 + ( 3 + 4 ))
                      = (+) 1 ((+) 2 7) -- 1 + ( 2 + 7 )
                      = (+) 1 9 -- 1 + 9
                      = 10




-}
--hence
{-
previous functions can be defined (partially applied def, this receives a list as argument obviously)
sum' :: [Int] -> Int
sum' = foldr (+) 0 -- We partially apply foldr


product' :: [Int] -> Int
product' = foldr (*) 1


and' :: [Bool] -> Bool
and' = foldr (&&) True

length' = foldr (\_ n -> 1 + n) 0 --lambda could be simplified to (\_ -> (+) 1)



reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

-}

--foldl is the same as foldr but goes through the list from left to right


{-

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] =  v
foldr f v (x:xs) = f x (foldr f v xs)


foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
-- f here is a function that receives type a and b and returns a

For example, let's see what happens step by step when we replace foldr with foldl in the sum function:


foldl (+) 0 [1,2,3,4] = foldl (+) ((+) 0 1) [2,3,4]
                      = foldl (+) ((+) ((+) 0 1) 2) [3,4]
                      = foldl (+) ((+) ((+) ((+) 0 1) 2) 3) [4]
                      = foldl (+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) []
                      = (+) ((+) ((+) ((+) 0 1) 2) 3) 4 -- ((( 0 + 1 ) + 2 ) + 3 ) + 4
                      = (+) ((+) ((+) 1 2) 3) 4 -- ((1 + 2 ) + 3 ) + 4
                      = (+) ((+) 3 3) 4 -- (3 + 3 ) + 4
                      = (+) 6 4 -- 6 + 4
                      = 10

-- the second argument 
 ((+) 0 1) and then ((+) ((+) 0 1) 2) etc

keeps growing while the third argument 
[2,3,4] then [3,4] etc

keeps getting smaller.

In foldr we start operating in the last element of the list, see previous example.
((+) 4 0)) is the first summation done.
In foldl we start operating in the first element of the list. first summation done is
((+) 1 2)


SOMETIMES OPERATIONS ARE NOT LEFT RIGHT SYMMETRIC OR ARE MORE INNEFICIENT IN ONE DIRECTION.

foldr (-) 0 [4,3,2,1] == foldl (-) 0 [4,3,2,1]
-- False

foldl (-) 0 [4,3,2,1] = (((0-4)-3)-2)-1 = -10
-- first element first to operate
foldr (-) 0 [4,3,2,1] = 4-(3-(2-(1-0))) = 2
-- last element first to operate

Inneficiency occurs in (++)

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

it goes through the whole first list before starting to add on the right. Since function reverse uses this
-- it might get really slow for big lists.
 

 Prepending one element (cons operator :) x:xs is always fast. list1 ++ list2 speed depends on size of list1.


reverse' = foldr (\x xs -> xs ++ [x]) []
reverse' [1,2,3] = foldr (\x xs -> xs ++ [x]) [] [1,2,3]
= (\x xs -> xs ++ [x]) 1 (foldr  (\x xs -> xs ++ [x]) [] [2,3])
= (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 (foldr  (\x xs -> xs ++ [x]) [] [3])
= (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 ((\x xs -> xs ++ [x]) 3 (foldr  (\x xs -> xs ++ [x]) [] []))
= (\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 ((\x xs -> xs ++ [x]) 3 [])
=(\x xs -> xs ++ [x]) 1 ((\x xs -> xs ++ [x]) 2 [3])
=(\x xs -> xs ++ [x]) 1 [3,2]
= [3,2,1]



And because now we can traverse the list from left to right, we can use the : (cons) operator to join values instead of ++.
Taking that into account, we can write reverse' like this:

reverse'' :: [a] -> [a] 
reverse'' = foldl (\x y -> y:x) []  -- Same as: foldl (flip (:)) []

reverse'' [1,2,3] = foldl (\x y -> y:x) [] [1,2,3]
= foldl (\x y -> y:x) ((\x y -> y:x) [] 1) [2,3]
= foldl (\x y -> y:x) ((\x y -> y:x) (\x y -> y:x) [] 1) 2) [3]
= foldl (\x y -> y:x) ((\x y -> y:x) (((\x y -> y:x) (\x y -> y:x) [] 1) 2)) 3) []
= foldl (\x y -> y:x) ((\x y -> y:x) (((\x y -> y:x) [1] 2)) 3) []
= foldl (\x y -> y:x) ((\x y -> y:x) [2,1] 3) []
= foldl (\x y -> y:x) [3,2,1] [] = [3,2,1]



reverse with foldr starts with [3] (goes from right to left) and ++ to the list
reverse with foldl starts with [1] (goes from left to right) and prepends (:) to the list

for big lists reverse with foldl is much faster.



foldl' IS DEFINED IN HASKELL.

MORE EFFICIENT VERSION OF foldl because it does intermediate steps before moving on with the recursion.

-- Same as:             (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [4])))
foldr (+) 0 [1,2,3,4] = 1 + (2 + (3 + (foldr (+) 0 [4])))
We see that we can't do much for foldr since we don't have a single operator with both arguments. So we'll always need to resolve the recursive function first.

But! If we take a look at the same intermediate step in foldl:

-- Same as:             foldl (+) ((+) ((+) ((+) 0 1) 2) 3) [4]
foldl (+) 0 [1,2,3,4] = foldl (+) (((0 + 1) + 2) + 3) [4]
We could totally reduce (((0 + 1) + 2) + 3) to 6 before continuing with the recursion!

this is what foldl' does .


We're going to give some rules of thumb from least to most used fold:

Use foldl:

Rarely.
If the combining function is lazy in its first argument. (foldl may return a result where foldl' hits an exception.)

Use foldl':

When the list to which it is applied is large but definitely finite, you do not care about the implicit reversal (for example, because your combining function is commutative), and you seek to improve the performance of your code.
When you actually do want to reverse the order of the list in addition to possibly performing some other transformation to the elements. (Taking advantage of the implicit reverse.)

Use foldr:

When transforming lists into lists with related elements in the same order.
When transforming infinite lists into other infinite lists. (If the function passed is lazy in its second argument, foldr will lazily generate the result, computing only as much as is required.)
When the folding function can short-circuit (terminate early) by yielding a result that does not depend on the value of the accumulating parameter.
If you're not sure.

see https://wiki.haskell.org/Foldr_Foldl_Foldl' for me

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

let num = 1 :: Int
print(num)
print(reverse' "stressed")
--print(repeat' num)
-- [17,17,17,...]
print(replicate' 0 True)
-- []
print(replicate (-1) True)
-- []
print(replicate 4 True)
-- [True,True,True,True]
print(concat' [[1,2],[3],[4,5,6]])
-- [1,2,3,4,5,6]
print(zip' [1] ['a', 'b'])
-- [(1,'a')]
print(zip' [1, 2] ['a'])
-- [(1,'a')]
--print(zip' [] [1..]) --works in ghci
-- []
--print(zip' [1..] []) --works in ghci
--[]
print(zipWith' (+) [1, 2, 3] [4, 5, 6])
print(zipWith' (,) [1] ['a','b'])


print(takeWhile' (< 3) [1,2,3,4,1,2,3,4])
-- [1,2]
print(takeWhile' (< 9) [1,2,3])
-- [1,2,3]
print(takeWhile' (< 0) [1,2,3])
-- []
print(func 5)
--print(snd orderList)
print(beercost orderList)