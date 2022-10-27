
-- Question 1
-- Write a multiline comment below.
{-
run in ghci
cd to directory of file
ghci
:l name_file
call functions on the terminal: mult3 6 (= 18)
:q to quit
-}
-- Question 2
-- Define a function that takes a value and multiplies it by 3.
mult3 :: Double -> Double
mult3 x = 3*x
-- Question 3
-- Define a function that calculates the area of a circle.
areacircle :: Double -> Double
areacircle r = pi * r * r
-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
volumecylinder :: Double -> Double -> Double
volumecylinder r h = areacircle r * h
-- Question 5
-- Define a function that checks if the volume of a cylinder is greater than or equal to 42.
testvolumecylinder :: Double -> Double -> Bool
testvolumecylinder r h = volumecylinder r h >= 42


main :: IO ()
main = do
{- comentario multiline
-}
--comentario oneline
-- compilar  ghc -o hw hw.hs
--correr ./hw
-- ctrl + space run in VS
-- ctr + S to save in VS 


print(mult3 6)
print(areacircle 3)
print (volumecylinder 3 6)
print (testvolumecylinder 3 6)


