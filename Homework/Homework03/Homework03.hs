-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
mcon :: Double -> Double -> Double -> String
mcon pph dh maxE
    | dh <0 || dh >24 =  "Insert a number of hours between 0 and 24."
    | pph<0 = "Insert a positive hourly consumption."
    | maxE<0 = "Insert a positive maximum monthly consumption."
    | otherwise = if mus > maxE then "Monthly usage is above the maximum allowed." else
        if mus < maxE then "Monthly usage is below the maximum allowed." else
            if mus == maxE then "Monthly usage is the maximum allowed." else  "Invalid inputs."
    where mus = pph * dh * 30





-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.
-- In the previous function, return the excess/savings of consumption as part of the message.

mcon2 :: Double -> Double -> Double -> String
mcon2 pph dh maxE
    | dh <0 || dh >24 =  "Insert a number of hours between 0 and 24."
    | pph<0 = "Insert a positive hourly consumption."
    | maxE<0 = "Insert a positive maximum monthly consumption."
    | otherwise = if mus > maxE then "Monthly usage is above the maximum allowed by " ++ show (mus-maxE) ++ " kWh." else
        if mus < maxE then "Monthly usage is below the maximum allowed by " ++ show (maxE-mus) ++ " kWh."  else
            if mus == maxE then "Monthly usage is the maximum allowed: " ++ show maxE ++ " kWh."  else  "Invalid inputs."
    where mus = pph * dh * 30


-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

--A: number of seconds in a given ammount of years.

yearsinsecs :: Double -> Double
yearsinsecs years=
    let minuteinsecs = 60
        hourinsecs = 60 * minuteinsecs
        dayinsecs = 24 * hourinsecs
        yearinsecs = 365.25 * dayinsecs
    in years*yearinsecs





-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  


--A: Question is a bit ambiguous. I will interpret this as writing a program that gives the quotient of 2 numbers, never returning a result bigger than 1 or zero.

divi :: (Num a, Eq a, Ord a, Show a, Fractional a) => a -> a -> String
divi num1 num2
    | big /= 0  = if small ==0 then "Smallest number is zero. Result is zero." else show (small/big)
    | otherwise = if small == 0 then "Both numbers are zero." else "Biggest number is zero. Result not greater than 1 and different from zero is impossible."
    where tuple = if num1 > num2 then (num1,num2) else (num2,num1)
          big = fst tuple
          small = snd tuple
-- first entry of tuple is always the biggest number.        

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of the square root of the product with the square root of the quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block. 


--A: changed 'sum of squares for the product and quotient of those numbers' to 'sum of the square root of the product with the square root of the quotient of those numbers'
--which is what I think it is meant. sum of squares means something different.

sumnums :: (Num a, Eq a, Ord a, Show a, Fractional a, Floating a) => a -> a -> a
sumnums num1 num2 = let sqrtp = sqrt prod where prod = num1*num2
                 in sqrtp + sqrtquot
                 where sqrtquot = let quoti = num1/num2 in sqrt quoti

-- very weird way of defining this function. I believe it is just to show how let and where can be used interchangeably, sometimes.


-------- NOTES ----------


func :: (Eq a, Num a, Num p) => a -> p
func a = if a == 0 then 2 else
    if a==1 then 3 else 0

func2 :: (Eq a, Num a, Num p) => a -> p
func2 a
  | a == 0 = 2
  | a==1 = 3
  | otherwise = 0



-- compares 2 temperatures, one in farenheit and the other in celsius.
-- prints the largest one, in Kelvin.


---- let
hotterinKelvin :: Double -> Double -> Double
hotterinKelvin c f =
    let fToC t = (t-32) * 5/9
        cToK t = t+ 273.16
        fToK t = cToK (fToC t)
    in if c > fToC f then cToK c else fToK f

-- let defs exist only within that let expression.
-- if previous func had another if after it, the functions defined in the previous let
-- would be out of scope.



---- where

hotterinKelvin' :: Double -> Double -> Double
hotterinKelvin' c f = if c > fToC f then cToK c else fToK f
    where  fToC t = (t-32) * 5/9
           cToK t = t+ 273.16
           fToK t = cToK (fToC t)

--or 

hotterinKelvin'' :: Double -> Double -> Double
hotterinKelvin'' c f
    | c > fToC f = cToK c
    | otherwise = fToK f
    where  fToC t = (t-32) * 5/9
           cToK t = t+ 273.16
           fToK t = cToK (fToC t)

-- where defs exist only in the function scope.


--- Let vs where 

-- Let is convenient whenever we want to split complex expressions into
-- smaller building blocks that you combine into a final expression.
-- State building blocks first and then combine them -> let.

-- Where is convenient when using guards. a where is acessible to all guards.
-- State conditions and then use definitions common to these conditions -> where.




main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 


-- other uses for let, inside an IO ()
let list = [1,2]
let s1 = 10
-- prefix integer division
print(func 1)
print(func2 2)
print(hotterinKelvin s1 100)
print(hotterinKelvin' 40 100)
print(hotterinKelvin'' 40 100)
print(mcon 30000 (-2) 500000)
print(mcon2 30000 2 500000)
print(yearsinsecs 1)

print(divi 0 (-20))
print(sumnums 3 2)