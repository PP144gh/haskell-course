-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use list literal pattern" #-}

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

-- this is a list of tuples. each tuple element consists of a list of Int. the list nested contains 2 tuples, each with a list of Int in each entry.
get4 :: [([Int], [Int])] -> String
get4 [(_,_:x:_),(_,_)] = "Print " ++ show x
get4 nlist = "Catch-all"


-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.

func :: [p] -> [p]
func (_:_:_:x) = x
func list = list


func' :: [p] -> [p]
func' list = case list of 
    (_:_:_:x) -> x
    _ -> list

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together

foo :: (Integer,Integer,Integer) -> Integer
foo (x,y,z) = x+y+z

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.



foo2 :: [p] -> Bool
foo2 [] = True
foo2 list = False
--foo3 _ = False also works because of the top down try.

foo2' :: [a] -> Bool
foo2' list = case list of
    [] -> True
    _ -> False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.

tailf :: [a] -> [a]
tailf [] = []
tailf (_:x) =x

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)

foo6 :: Int -> Int
foo6 n = n + 
    case n `mod` 2 of
      0 -> 1
      _ -> 0

-- or
foo6' :: Int -> Int
foo6' n = n + 
    case even n of
      True -> 1
      False -> 0

-------------- NOTES ----------


-- pattern matching

specialBirthday :: Int -> [Char]
specialBirthday 1   = "First birthday!"
specialBirthday 18  = "You're an adult!"
specialBirthday 60  = "finally, I can stop caring about new lingo!"
specialBirthday age = "Nothing special, you're just " ++ show age

-- last line is a catch-all pattern, works like otherwise in guards.
-- matches from top to bottom, like guard condition checks.
-- Note the binding of the value of age in the output of the function by using show func.


-- cons : operator

-- (:) :: a -> [a] -> [a]

-- 3 : [4,5]  -- [3,4,5]

-- 'L' : "ook, mom! I'm programming" -- "Look, mom! I'm programming"


-- how compiler sees lists

-- [1,2,3,4] == 1:2:3:4:[]  -- True


-- pattern matching lists

whatsInsideThisList :: [Int] -> String
whatsInsideThisList []         = "It's empty!"
whatsInsideThisList [x]        = "A single element: " ++ show x
whatsInsideThisList [x, y]     = "Two elements: " ++ show x ++ " and " ++ show y
whatsInsideThisList (x:y:z:[]) = "The list has three elements: " ++ show [x,y,z]
whatsInsideThisList (x:rest)   = "The first element is: " ++ show x ++ ", and there are quite a few more!"

-- last line is the catch-all pattern matching

-- whatsInsideThisList []           -- "It's empty!"
-- whatsInsideThisList [1, 2]       -- "Two elements: 1 and 2"
-- whatsInsideThisList [1, 2, 3]    -- "The list has three elements: [1,2,3]"
-- whatsInsideThisList [1, 2, 3, 4] -- "The first element is: 1, and there are quite a few more!"

-- pattern matching of only what you need
firstAndThird :: [Bool] -> String
firstAndThird (x:_:z:_) = "The first and third elements are: " ++ show x ++ " and " ++ show z
-- ignoring 2nd element and the rest. keeping 1st and 3rd.

firstAndThird _ = "Don't have them!"
-- if previous pattern doesnt match, i.e. list has less than 3 elements, this triggers.
--firstAndThird [True, True, False]

-- another example

initials' :: String -> String -> String  
initials' (f:_) (l:_) = [f] ++ "." ++ [l] ++ "." 
initials' _ _ = "How was your name again?"

--initials' "Nikola" "Tesla"

--output is N.T. 

-- pattern matching tuples

firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x

--firstOfThree (1,2,3) -- output: 1

pairFromFour :: (a, b, c, d) -> (b, d)
pairFromFour (_, x, _, y) = (x, y)

--pairFromFour (1,2,3,4) -- output (2,4)


-- Case Expressions 
convertNuc ::  Char -> Char
convertNuc t = case t of 'G' -> 'C'
                         'C' -> 'G'
                         'T' -> 'A'
                         'A' -> 'U'
                         t -> 'N'

-- case expression w/ pattern matching
checkForZeroes :: (Int, Int, Int) -> String
checkForZeroes tuple3 = case tuple3 of
  (0, _, _) -> "The first one is a zero!"
  (_, 0, _) -> "The second one is a zero!"
  (_, _, 0) -> "The third one is a zero!"
  _         -> "We're good!"
  
-- checkForZeroes (32,0,256) -- output: "The second one is a zero!"

--  At its core, pattern matching on parameters in function definitions is just syntactic sugar for case expressions! 
--So, the previous code is interchangeable with this one:
checkForZeroes' :: (Int, Int, Int) -> String
checkForZeroes' (0, _, _) = "The first one is a zero!"
checkForZeroes' (_, 0, _) = "The second one is a zero!"
checkForZeroes' (_, _, 0) = "The third one is a zero!"
checkForZeroes' _         = "We're good!"

--Because now we're using case EXPRESSIONS, we can use them anywhere an expression can be used! Not only when defining a function.
-- So, for example, we can concatenate the result of evaluating the case expression with another String:
checkForZeroes'' :: (Int, Int, Int) -> String
checkForZeroes'' tuple3 = "The " ++ show tuple3 ++ " has " ++
    case tuple3 of
      (0, _, _) -> "a zero as its first element"
      (_, 0, _) -> "a zero as its second element"
      (_, _, 0) -> "a zero as its third element"
      _         -> "no zeroes!"

-- checkForZeroes'' (32,0,256) output: "The (32,0,256) has a zero as its second element"
--`case` expressions convenient to use inside other expressions. 
-- anything that you can do with `case` expressions can be done by defining functions with `let`, `where`, or guards.


-- Declaration style vs Expression style

--There are two main styles for writing functional programs in Haskell:

{-
- The **declaration style** is where you formulate an algorithm in terms of several equations to be satisfied.
ex: write algorithm and then define stuff in a where below
- The **expression style** is where you compose big expressions from small expressions.
ex: define some building blocks in a let and then write the algorithm which uses every building block.

| Declaration style                                      | Expression style                                    |
|--------------------------------------------------------|-----------------------------------------------------|
| `where` clause                                         | `let` expressions                                   |
| Pattern matching in function definitions: `f [] = 0`   | case expression: `f xs = case xs of [] -> 0`        |
| Guards on function definitions: `f [x] \| x > 0 = 'a'` | `if` expression: `f [x] if x > 0 then 'a' else...`  |
| Function arguments on left-hand side: `f x = x*x`      | Lambda abstraction: `f = \x -> x*x`                 |


-- Declaration style is more similar to how mathematics is written by humans
-- Expression style is more like the compiler thinks.

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

let list =[1..5]
let list2 = [1..3]
let list3 = [1..2]
let list4 =[1]

let list5="abd"

print(get4 nested)
print(func list)
print(func list2)
print(func list3)
print(func' list)
print(func' list2)
print(func' list3)
print(foo (1,2,3))

print(foo2 [2,2])
print(foo2' [2,2])

print(tailf [] :: [Int])
print(tailf list5)


print(foo6 4)
print(foo6 3)


print(foo6' 4)
print(foo6' 3)

