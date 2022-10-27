import Data.Typeable
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.


-- Floating is a class. subclass of Fractional. Fractional objets are numbers that have the division operation. Floating is a class of number types that are complete metric splaces. 
--Since you can define the real numbers as limits of sequences of rational numbers (think of taylor series for pi)
--any Floating type is able to represent (again, at least to a decent approximation) any real number.
-- exponentiation ** only works for Floating?
f1 :: (Num a, Floating a) => a -> a -> a -> a
f1 x y z = x ** (y/z)

f2 :: (Num a, Ord a, Floating a) => a -> a -> a -> a
f2 x y z = sqrt (x/y - z)

-- prepends char x to string (list of chars) (show y) ++ z . show y converts number y into string. instances of Show can be any type: 	(), [a], Char, Double, Float, Int, Integer,
f3 :: Show a => Char -> a -> String-> String
f3 x y z = x:(show y ++ z)

-- Ord is fundamental here because of the evaluation of x > y
f4 :: (Num a, Ord a)  => a -> a -> Bool -> [Bool]
f4 x y z = [x > y] ++ [z]

-- Eq class defines equality and inequality betweem instances.
f5 :: (Eq a) => [a] -> [a] -> [a] -> Bool
f5 x y z = x == (y ++ z)

-- Question 2
-- Are really all variables in Haskell immutable? Try googling for the answer.
--A: No, see https://wiki.haskell.org/Mutable_variable


-- Question 3
-- Why should we define type signatures of functions? How can they help you? How can they help others?
--A: protects us from ourselves. helpful for planning, can devise flow of types prior to writing the functions. easier to read code for others.


-- Question 4 
-- Why should you define type signatures for variables? How can they help you?
--A: Same as question 3. no very ommon since variables are usually defined inside functions with its type infered.


-- Question 5
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
--A: show function transforms any type into string. read does the opposite. round convertes double to Int.


-- Question 6
-- How would you write the prod function from our lesson so that it works for Int and Double? Does the code compile?

--polymorphic types
--first :: (a,b) -> ab --tuple with type a and type b and returns type a
--first (x,y) = x
-- fst and snd functions. snd returns 2nd element of a pair.


--prod :: Num a => a -> a -> a -- means for a numerical type a it works with type a -> type a -> type
prod :: Num a => a -> a -> a -- means for a numerical and with ordering (no complex numbers for instace) type a it works with type a -> type a -> type
prod x y = x * y
-- Num is a Haskell class.


-- Question 7
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
--A: result of words "hi joe" is ["hi","joe"] is a list of strings. since string is a list of chars, this is a list of a list.

--A: innermost elements:
-- ["hi","joe"] = [['h','i'],['j','o','e']]
-- to get the o from joe, for instance, one should do ["hi","joe"]  !! 1 !! 1



-- function without parameters is a "variable". "variable" = definition or name in haskell because it is fixed, it doesnt "vary".
number :: Double
number = undefined

text :: String
text = "aaaa"

-- error because definition is fixed.
--text = "bbb"

square :: Double -> Double
square v = v*v


squareInt :: Int -> Int
squareInt v = v*v


-- Tuples. fixed size. no concatenation like lists. single element tuple = element
tuple :: (Char, Double, Int)
tuple = ('a', 23.0, 2)


list :: [Int]
list = [1,3,4]



main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 




-- :type variable in ghci
print(typeOf number)
print(typeOf text)
print(square 2)
print(square 2.3)
print(squareInt 2)
--print(squareInt 2.3)
--print(square 2 == squareInt 2)
-- prefix integer division
print(div 3 2)


-- prefix notation func arg and infix notation 2 + 2 or 3 div 2
-- function with a symbol name is by def an infix function
-- infix double division
print(3/2)
-- use infix as prefix
print((/) 3 2)
--use prefix as infix
print (3 `div` 2) -- shift right accent
print(typeOf (square 2.3))


print (2^64 ::Int) -- better performance but capped at 2^62

print (2^64 ::Integer)

print(['H','i','!'] == "Hi!") -- String == [Char] "A" String 'A' char


-- float less precise but better memory usage than double


print(tuple)

print(list !! 0)
print(list !! 1)
print(list !! 2)

print (['a','c'..'z'])

--print ([1..])

print(take 3 [2..])

--- lists

--cons operator :
print(2 : [3,4,5]) -- adds element to beggining of list. instantaneous

--concatenation operator ++
print([2,3,4] ++ [5,6]) -- compiler runs trought all of left list.


print(length [2,2,2,2])
print(null [2,2,2,2])
print(sum [2,2,2,2])
print(1 `elem` [2,2,2,2,2]) -- checks if 1 is in the list

--joining and breaking text
--words , unwords, lines, unlines
print(words "hi joe")
let test = ["hi", "joe"]
print(test !! 1 !! 1)
print(lines "hi joe \n hi peter")

--let y = [2,3]
--print (f3 'a' y "lol")


