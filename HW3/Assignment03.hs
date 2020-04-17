module Assignment03 where

import RegEx
import SLG

import Data.List (nub)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
data Numb = E | S Numb deriving Show


bigrams :: [a] -> [(a, a)]
bigrams list = zip list (tail list) 


pretty :: (Eq a) => [(a, a)] -> [a]
pretty l = case l of
   [] -> []
   x:xs -> if (isChained l) then nub ((fst x):(snd x): pretty xs) else []
 

follows :: (Eq sy) => SLG sy -> sy -> [sy]
follows (start, final, trans) a =
    case trans of
     [] -> []
     x:xs -> if ((fst x) == a) then (snd x): (follows (start, final, xs) a) else follows (start, final, xs) a
-- follows checks if the first element of the head of trans matches a, if so return the snd element of x. 
-- if not, continue iterating on the rest of trans. 

   

precedes :: (Eq sy) => SLG sy -> sy -> [sy]
precedes (start, final, trans) a = 
    case trans of 
        [] -> []
        x:xs -> if ((snd x) == a) then (fst x): (precedes (start, final, xs) a) else precedes (start, final, xs) a
-- basically like follows but checking the second element in the head bigram against a. 



-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very","very","very"],["very","very","fat"],["very","fat","cat"],
--    ["very","very"],["very","fat"]]


forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward g n q = let (start, final, trans) = g in 
   


-- if g matches a transition head, 
-- get a follows list from there. 


-- follows obtains a list. 
-- base case is n = 0 



-- nub removes duplicates in a list
-- concat takes list of lists and concatenates them. 

-- so forward should take x, 

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
occurrences n r = undefined

optional :: (RegEx a) -> (RegEx a)
optional = undefined

------ Discussion Section -------
applyToAll :: (a -> a) -> [a] -> [a]
applyToAll = \f -> \l -> case l of 
    [] -> []
    x:xs -> (f x) : (applyToAll f xs)

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