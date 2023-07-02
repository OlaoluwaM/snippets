module Play where

import Data.List
import System.IO

isInteger n = n == fromInteger (round n)

powOfThrees = [fromInteger $ round x | x <- [1 .. 100], isInteger (logBase 3 x)]

toLogBase3 :: [Double] -> [Integer]
toLogBase3 = map (fromInteger . round . logBase 3)

multiTable = [["x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z | x <- [1 .. 10], y <- [1 .. 10], z <- [1 .. 10]]]

getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems [x] = "Your list starts with " ++ show x
getListItems [x, y] = "First item in list: " ++ show x ++ " Second item in list: " ++ show y
-- .. and so on
-- You can also do something similar to the rest operator in Typescript
getListItems all@(x : xs) = "First item: " ++ show x ++ " Rest: " ++ show xs ++ ", everything: " ++ show all

times4 :: Int -> Int
times4 x = x * 4

listTimes4 :: [Int] -> [Int]
listTimes4 = map times4

-- Recursive map def
multBy4 :: [Int] -> [Int]
multBy4 xs = map times4 xs

type Age = Integer

data SchoolGrade = Kindergarten | Elementary | HighSchool | College deriving (Eq, Show)

getSchoolGrade :: Age -> SchoolGrade
getSchoolGrade age = case age of
  5 -> Kindergarten
  6 -> Elementary
  7 -> HighSchool
  _ -> College

data Person = MkPerson
  { name :: String
  , age :: Integer
  , isStudent :: Bool
  }
  deriving (Show)

aPerson = MkPerson{name = "Ola", age = 20, isStudent = True}

personName :: Person -> String
personName = name

olaTenYearsInTheFuture :: Person
olaTenYearsInTheFuture = aPerson{age = age aPerson + 10, isStudent = False}

data Bar = MkBar {bar :: String, isBar :: Bool}

data Baz = MkBaz {baz :: String, isBaz :: Bool}

data Foo = MkFoo Baz | MkFoo' Bar

f = MkFoo' $ MkBar{bar = "ola", isBar = True}

main = do
  putStrLn "What's you're name"
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"

writeToFile = do
  fileToWrite <- openFile "sample.txt" WriteMode
  hPutStrLn fileToWrite "Some text"
  hClose fileToWrite

readFileContents = do
  fileToRead <- openFile "sample.txt" ReadMode
  fileContents <- hGetContents fileToRead
  putStrLn fileContents
  hClose fileToRead
