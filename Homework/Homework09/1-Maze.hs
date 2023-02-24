{-

**************************** IMPORTANT ****************************

This week is a two-step homework. First, you have to solve the
"Maze" challenge, and then the "Forest" challenge. The challenges
are in two separate files in both the homework and solution, so
you can check the solution for the first "Maze" challenge without
spoilers of the "Forest" one. Make sure to check the solution for
"Maze" (and only "Maze," I see you 🥸👀) before starting with the
"Forest" challenge!

*******************************************************************

Today, you'll build the simplest and most basic game imaginable.
It'll be a maze game where the player has to write a list of moves, and the game will perform them
and tell the player where it ends up. Then, the player can change the moves and check again until it
finds the exit.

To play the game, the player will open GHCi, load this file, and run a "solveMaze" function that
takes a maze and a list of moves and returns a String with the resulting state.

It should look like this:

*Main> solveMaze testMaze []
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoLeft]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward]
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoForward, GoRight]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward, GoLeft]
"YOU'VE FOUND THE EXIT!!"

How are you going to achieve this? You can try it on your own, but here you have a
step-by-step just in case:

1. Write two data types. One for the moves (Move) you can make, and another for the maze (Maze).
(Use the example above to figure them out.)

2. Write a function called "move" that takes a maze and a move and returns the maze after the move.

3. Write a "testMaze" value of type "Maze" and test the "move" function in GHCi.

4. Write the "solveMaze" function that will take a maze and a list of moves and returns the maze
after making those moves.

5. If you test the "solveMaze" function, you'll see that each time you try to solve the maze,
it'll print the whole maze for the player to see. To avoid that, write a "showCurrentChoice" function
that takes a maze and returns a different string depending on if you hit a wall, found the exit, or
still need to make another choice.

6. Adapt adapt "solveMaze" function to use "showCurrentChoice" and play with your new game using GHCi! :D
-}



{-
--1sr try



data Move = GoLeft | GoRight | GoForward deriving (Show,Eq)

infixr 5 :->
data Maze a = End | HitWall | a :-> (Maze a) deriving (Show,Eq)

testMaze :: Maze Move
testMaze= GoForward :-> GoLeft :-> End


move :: Maze Move-> Move -> Maze Move
move End _ = End
move HitWall _ = HitWall
move (y :-> ys) moove
    | y == moove = ys 
    | otherwise = HitWall


solveMaze:: Maze Move -> [Move] -> String
solveMaze maze xs = showCurrentChoice (foldl move maze xs) 


showCurrentChoice :: Maze Move -> String
showCurrentChoice maze 
    | maze == HitWall = "You've hit a wall!"
    | maze == End = "YOU'VE FOUND THE EXIT!!"
    | otherwise = "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

-}


{-
prof solution


-- Step 1
data Move = GoForward | GoLeft | GoRight

data Maze = FoundExit | HitWall | Passage Maze Maze Maze deriving (Show)

-- Step 2
move :: Maze -> Move -> Maze
move HitWall _ = HitWall
move FoundExit _ = FoundExit
move (Passage x _ _) GoLeft = x
move (Passage _ x _) GoForward = x
move (Passage _ _ x) GoRight = x

-- Step 3
testMaze :: Maze
testMaze = Passage HitWall (Passage FoundExit HitWall HitWall) (Passage HitWall (Passage HitWall HitWall HitWall) HitWall)

-- Step 4
solveMaze' :: Maze -> [Move] -> Maze
solveMaze' = foldl move

-- Step 5
showCurrentChoice :: Maze -> String
showCurrentChoice HitWall = "You've hit a wall!"
showCurrentChoice FoundExit = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice _ = "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

-- Step 6
solveMaze :: Maze -> [Move] -> String
solveMaze l m = showCurrentChoice $ foldl move l m


-}




--2nd try



data Move = GoLeft | GoRight | GoForward deriving (Show,Eq)


data Maze = End | HitWall | Passage Maze Maze Maze  deriving (Show,Eq)

testMaze :: Maze 
testMaze = Passage HitWall (Passage End HitWall HitWall) (Passage HitWall (Passage HitWall HitWall HitWall) HitWall)


move :: Maze-> Move -> Maze
move End _ = End
move HitWall _ = HitWall
move (Passage x _ _) GoLeft = x
move (Passage _ x _) GoForward = x
move (Passage _ _ x) GoRight = x


solveMaze:: Maze -> [Move] -> String
solveMaze maze xs = showCurrentChoice (foldl move maze xs) 


showCurrentChoice :: Maze -> String
showCurrentChoice maze 
    | maze == HitWall = "You've hit a wall!"
    | maze == End = "YOU'VE FOUND THE EXIT!!"
    | otherwise = "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

-- my 1st solution didn't allow for a complex maze where paths bifurcate. It only solved the problem for a maze that has only one way to go, and all the other options are walls.
-- The data definition of maze could be generalised to allow for a more complex maze, but all possible paths would have to be defined in the test maze, starting from the first step, making it very cumbersome for
-- very complex mazes.

main :: IO ()
main = do


--let moves = [GoForward]
--let moves2 = [GoLeft,GoForward]

{-
*Main> solveMaze testMaze []
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoLeft]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward]
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveMaze testMaze [GoForward, GoRight]
"You've hit a wall!"
*Main> solveMaze testMaze [GoForward, GoLeft]
"YOU'VE FOUND THE EXIT!!"
-}

print(solveMaze testMaze [])
print(solveMaze testMaze [GoLeft])
print(solveMaze testMaze [GoRight])
print(solveMaze testMaze [GoRight,GoForward])
print(solveMaze testMaze [GoRight,GoForward,GoForward])


print(solveMaze testMaze [GoForward])
print(solveMaze testMaze [GoForward, GoRight])
print(solveMaze testMaze [GoForward, GoLeft])


print(solveMaze testMaze [GoRight])
print(solveMaze testMaze [GoRight, GoLeft])
print(solveMaze testMaze [GoRight, GoForward, GoLeft])

--print(move testMaze (head moves))
--print(solveMaze testMaze moves)
--print(solveMaze testMaze moves2)