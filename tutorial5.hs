-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where

import Data.Char
import Data.Ratio
import Test.QuickCheck

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles xs = concat (map f xs)
    where f x = [x,x]

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map (/100) (map fromIntegral xs)

-- c.
uppersComp :: String -> String
uppersComp str = map toUpper str


-- 2. Filter
-- a.
alphas :: String -> String
alphas str = filter isAlpha str

-- b.
above :: Int -> [Int] -> [Int]
above k xs = filter (f k) xs
    where f k x = k < x 

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter f xs
    where f (x,y) = x /= y

-- d.
rmChar:: Char -> String -> String
rmChar c str = filter (f c) str
    where f c x = c /= x

-- e.
rmCharComp :: Char -> String -> String
rmCharComp c str = [x | x <- str, c /= x]

prop_rmChar:: Char -> String -> Bool
prop_rmChar c str = rmChar c str == rmCharComp c str

-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (2*) (filter (>3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' xs = map reverse (filter f xs)
    where f x = even (length x)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold f = foldr (&&) True f

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs 

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] ys = ys
rmCharsRec (x:xs) ys 
    |xs == [] = rmChar x ys
    |otherwise = rmCharsRec xs (rmChar x ys) 

rmCharsFold :: String -> String -> String
rmCharsFold xs ys = foldr rmChar ys xs 

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform (x:xs) = all (x==) (xs)

-- b.
valid :: Matrix -> Bool
valid m = uniform (map length m) && length (head m) > 0 && length m > 0 


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length (head m)

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2
    |matrixHeight m1 == matrixHeight m2 && and(map valid [m1,m2]) && matrixWidth m1 == matrixWidth m2 = zipWith plusRow m1 m2
    |otherwise = error "Bro, I can't do that"
        where
            plusRow:: [Rational] -> [Rational] -> [Rational]
            plusRow r1 r2 = zipWith (+) r1 r2

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM m1 m2 
    |matrixWidth m1 == matrixHeight m2 && and (map valid [m1,m2]) = [[helper (m1!!k) ((reverser m2)!!n) | n<-[0..length m1 -1]] | k<-[0..length m1 -1] ]
    |otherwise = error "Wtf, man, do you even know what you're doing???"
        where
            helper:: [Rational] -> [Rational] -> Rational
            helper l1 l2 = sum (zipWith (*) l1 l2) 

reverser:: Matrix -> Matrix
reverser m =  [map (!!k) m | k <- [0..length (head m) -1]] 

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | x <-xs, y <- ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

-- -----------------------------------
-- -----------------------------------
-- -- Optional material [ ]
-- -----------------------------------
-- -----------------------------------
-- -- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f m = map (map f) m

zipMatrixWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrixWith f m1 m2 = mapMatrix (uncurry f) (zipMatrix m1 m2)

zipMatrix ::[[a]] -> [[b]] -> [[(a,b)]]
zipMatrix m1 m2 = [zip (m1!!k) (m2!!k) | k<-[0..length m1 -1]]  

indexedMatrix:: Matrix -> [[((Int,Int),Rational)]]
indexedMatrix m = zipMatrix (indexMatrix (matrixHeight m) (matrixWidth m)) m

indexMatrix:: Int -> Int -> [[(Int,Int)]]
indexMatrix m n = helperRec n [(x,y) | x<-[1..m], y<-[1..n]]
    where 
        helperRec:: Int -> [(Int,Int)] -> [[(Int,Int)]]
        helperRec _ [] = []
        helperRec n l = take n l: helperRec n (drop n l)


-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes xs = [x ++ (tail y) |k <- [0..length xs-1], let (x,y) = splitAt k xs] 

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = splitEach (matrixHeight m) [minor m (i,j) | i<-[1..matrixHeight m], j<-[1..matrixWidth m]]
--    where
minor:: Matrix -> (Int,Int) -> Matrix
minor m (i',j') = if matrixHeight m < 3 then m else splitEach (matrixHeight m -1)[ k | ((i,j),k) <- (concat (indexedMatrix m)), i/=i', j/=j' ]

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant m = (minors m)

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined

splitEach :: Int -> [a] -> [[a]]
splitEach _ [] = []
splitEach n xs = take n xs : splitEach n (drop n xs)
