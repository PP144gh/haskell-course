-- This homework is around creating Haskell types that represent wines from over the world.

-- Question 1
-- Different wines are made from different grapes, there are around 10000 varieties over the world!
-- Create a type synonym called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese", "Cabernet-sauvignon", "Merlot" and "Garnacha".

type Grape = String

gnames :: [Grape]
gnames =["Sangiovese","Cabernet-sauvignon", "Merlot", "Garnacha"]


-- Question 2
-- The most famous regions that export wine are located in France, Italy and Spain.
-- Each of these countries is divided up in smaller regions.
-- These smaller regions are known for a certain style, for example the Champagne region in France
-- Create a type synonym called "Region" for wine region given their country and region as a tuple of strings.
-- Additionally, use this type synonym for the regions: Bordeaux in France, Tuscany in Italy and Rioja in Spain.
type Region =( String, String)

regions :: [Region]
regions = [("Bordeaux", "France"),("Tuscany","Italy"),("Rioja","Spain")]

-- Question 3
-- A wine is either one of three kinds, these are red, white or rose wine.
-- Besides its kind, each wine also has a given alcohol level.
-- Create a data type called "Kind" that represents these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with 14.5% alcohol, white wine with 13% alcohol 
-- and Rose wine with 12% alcohol.

data Kind = Red {level :: Double} | White {level :: Double}| Rose {level :: Double}
    deriving (Show)

kinds :: [Kind]
-- kinds = [Red 14.5, White 13.0, Rose 12]
kinds = [Rose 14, Red 13.5, Red 12.5]
-- Question 4
-- In the world of wines, bottles display all of the above information for the consumer on its label.
-- Create a record type called "Label" that captures the grapes that are in a whine, the region its from,
-- and it's kind. Notice that some wines are a blended combination of multiple grapes!
-- Additionally, create for each of the described wine below a label.

-- Larrosa Rose is a rose wine from the region Rioja. It is made from the Garnacha grape and 
-- has a alcohol level of 14%.

-- Castiglioni is a red wine from the region of Tuscany. It is made from the grape Sangiovese and
-- has an alcohol level of 12.5%.

-- Bordeaux is known for its red wine, these are mainly a blend between Cabernet-sauvignon and Merlot.

-- Create a Label for the wine "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.

names :: [String]
names = ["Larrosa Rose","Castiglioni", "Le Petit Haut Lafitte"]
data Label = Label { name :: String, grapes :: [Grape], region :: Region, kind :: Kind} deriving (Show)

larrosaRose :: Label
larrosaRose = Label {name = head names, grapes = [gnames !! 3], region = regions !! 2, kind = head kinds}

castiglioni :: Label
castiglioni= Label (names !! 1) [head gnames] (regions !! 1) (kinds !! 1)

lePetitHaitLafitte :: Label
lePetitHaitLafitte = Label {name = names !! 2, grapes = [gnames !! 1, gnames !! 2], region = head regions, kind = kinds !! 2}


-- Question 5
-- Write a function `containsGrape` that takes a list of Labels and a Grape and returns a boolean.
-- The function should check if the there exists a wine in the Label that contains this Grape.

-- This is a test list for the `containsGrape` function with an grape that is not in the list.
grapeList :: [Label]
grapeList = [larrosaRose,castiglioni,lePetitHaitLafitte]

newGrape :: Grape
newGrape = "Pinot Noir"
--gives False

--newGrape = gnames !! 2
-- gives True

containsGrape :: Grape -> [Label] -> Bool
--containsGrape nGrape = foldr ((||) . (\label -> nGrape `elem` grapes label)) False
containsGrape nGrape = foldr ((||) . (\Label{grapes=xs} -> nGrape `elem` xs)) False

--prof solution
-- containsGrape labels grape = any (\Label{grapes=xs} -> grape `elem` xs) labels


main :: IO ()
main = do


print(containsGrape newGrape grapeList)



-- MY NOTES ---

-- example of new types and algebraic data types


{-


type Point = (Float,Float)
type Radius = Float
type Width = Float
type Height = Float
type Color = String

data Shape
    = Circle Point Radius Color
    | Rectangle Point Width Height Color



-}

-- Record Syntax and functions already defined


{-

data Employee = Employee { name :: String, experienceInYears :: Float } deriving (Show)

define instances as
richard = Employee { name = "Richard", experienceInYears = 7.5 }


name richard
"Richard"


experienceInYears richard
7.5

or this way

matt = Employee "Matt" 5

update value
newMatt = matt { experienceInYears = 6 }
newMatt



team = [Employee "John" 4, Employee "Josh" 2, Employee "Matthew" 7]

combinedExp :: [Employee] -> Float
combinedExp = foldr (\e acc -> experienceInYears e + acc) 0

combinedExp team

better way of doing shape


data Shape
  = Circle
      { position :: (Float, Float)
      , radius   :: Float
      , color    :: String
      }
  | Rectangle
      { position :: (Float, Float)
      , width    :: Float
      , height   :: Float
      , color    :: String
      }
  deriving (Show)


circ = Circle { position = (1, 2), radius = 6, color = "Green" }
:t circ
circ

rect1 = Rectangle (9, 3) 7 3 "Yellow"
:t rect1
rect1


position circ

color rect2

(1.0,2.0)
"Yellow"


area :: Shape -> Float
area (Circle _ r _) = pi * r ^ 2
area (Rectangle _ w h _) = w * h

area circ
area rect1

this can be done as

area :: Shape -> Float
area Circle {radius=r} = pi * r^2
area Rectangle {width=w,height=h} = w * h

area circ
area rect1


We pattern match on record-syntax value constructors by writing the constructor's fields between curly brackets and binding them to a variable on the right side of the field's equal sign.

What's interesting is that we only match the patterns of the fields we need to use. And this gives us another fantastic perk of record syntax. If we add another field to the data type, we don't need to change any of our previous functions! Because we don't take into account unused fields in our pattern matching!

Awesome, right?

Record syntax is especially useful when you have a data type with maybe dozens of fields. Like a type that contains the settings of an application. Or one that contains all the fields of a survey.

It allows you to use the type without the need to remember which value was what (because they are all named) and allows you to update and reference specific fields, ignoring the rest. So, if you change your type in the future, only the values and functions that use the changed field are affected. If any.
-}