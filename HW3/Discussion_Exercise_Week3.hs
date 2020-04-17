module Assignment03 where

import RegEx
import SLG

import Data.List (nub)
data Numb = E | S Numb deriving Show
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

bigrams :: [a] -> [(a, a)]
bigrams list = zip list (tail list) 


applyToAll :: (a -> a) -> [a] -> [a]
applyToAll = \f -> \l -> case l of 
    [] -> []
    x:xs -> (f x) : (applyToAll f xs)
-------------------------------------------------------------------------------

suffixes :: [a] -> [[a]]
suffixes = \l -> case l of
    [] -> []
    x:xs -> (x:xs) : (suffixes xs) 


beforeAndAfter :: (a -> b) -> [a] -> [(a,b)]
beforeAndAfter f [] = []
beforeAndAfter f (x:xs) = (x, f x) : (beforeAndAfter f xs) 


inverseMap :: [(a -> b)] -> a -> [b]
inverseMap [] x = []
inverseMap  (f:fs) x = f x : (inverseMap fs x)


countEvens :: Integral a => [[a]] -> [Int]
countEvens a = map (\x -> length (filter (\n -> n `mod` 2 == 0) x)) a

-- test: countEvens [[1,2,3,4,5,6], [2,3,4,6]]

oneStep :: Int -> [Int]
oneStep x = [2*x + 1, x*x + x]

reachable :: Numb -> Int -> [Int]
reachable E x = [x]
reachable (S n) x = concat (map (reachable n) (oneStep x))

-- take n list => returns first n-elements of list. 
-- Lists and strings can be concatenated the same 
-- $ sign serves as parenthesis for items after it. 

----------------------------------

follows :: (Eq sy) => SLG sy -> sy -> [sy]
follows = undefined

precedes :: (Eq sy) => SLG sy -> sy -> [sy]
precedes = undefined

-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very","very","very"],["very","very","fat"],["very","fat","cat"],
--    ["very","very"],["very","fat"]]

forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward = undefined

-- MORE EXAMPLE USAGE:
-- backward g2 1 "cat"
-- => [["cat"],["the","cat"],["fat","cat"]]
-- backward g2 2 "very"
-- => [["very"],["the","very","very"],["very","very","very"],["the","very"],
--    ["very","very"]]

backward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
backward = undefined

generates :: (Eq sy) => SLG sy -> Int -> [[sy]]
generates = undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

occurrences :: Int -> (RegEx a) -> (RegEx a)
occurrences = undefined

optional :: (RegEx a) -> (RegEx a)
optional = undefined
