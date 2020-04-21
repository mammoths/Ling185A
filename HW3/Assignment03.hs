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


addTo :: (Eq sy) => SLG sy -> [sy] -> sy -> Int -> [sy]
addTo g x y n = let next = follows g y in 
    case n of
    0 -> x 
    n' -> (addTo g x (head (appendNext g x y)) (n-1) ++ (appendNext g x y))
        

appendNext :: (Eq sy) => SLG sy -> [sy] -> sy -> [sy]
appendNext g x y = let next = follows g (head x) in
    case next of
       [] -> [head next]
       x:xs -> [head next]


-- We are appending "the" then the value, then essentially y value.
-- then... append the head of y's followers.
-- x is our OG symbol we wanna move forward from...
-- X is our value from  the [followers], each element. 

forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward g 0 q = [[q]]
forward g n q = forward' g n [q] 


forward' :: (Eq sy) => SLG sy -> Int -> [sy] -> [[sy]]   
forward' g n xs = let (start, final, trans) = g in
     let followers = follows g (head xs) in 
         case n of
             0 -> [followers]
             n' -> map (\x ->  (addTo g xs x n)) (concat (forward' g (n-1) xs))
                





---- Class Lecture 4/21/2020
--Wrapper Function
f :: Int -> Int -> [Int]
f n x = nub $ f' n [x] 


f' :: Int -> [Int] -> [Int]
f' _ [] = []
f' 0 xs = xs
f' n xs = 
    addTwos ++ minusFives
   where
     input = f' (n-1) xs
     addTwos = map (\y -> y + 2) input
     minusFives = map (\y-> y - 5) input

-- FORWARD calls AddTo, in the map, to add a value ("the") to each value of [followers]. 
-- 

-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very","very","very"],["very","very","fat"],["very","fat","cat"],
--    ["very","very"],["very","fat"]]

--make a new function that appends "the" ++ x, x being the result of followers. 
-- ESSENTIALLY, forward should return it's base case [followers] if 0, and if not zero, then it should:
    -- MAP to each value obtained from recursing through forwards (to get base case... ["cat", "very", "fat"])
     -- then append the element from basecase to the element that precedes it in a new list, and reverse it.
     -- This does not work for all cases. 


{-let (start, final, trans) = g in 
   let int = n in 
   let next = head (follows g q) in 
    case int of
        0 -> [follows g q]
        int' -> (follows g next) : forward g (n-1) q
-}


{- -- This forwards function does not give the correct output. only passes one test case. 
-- gonna need helps. Returns correct output only for n=1 because of hacky solution.

let followers = follows g (head xs) in
     let heirs = concat [follows g (head (tail followers))] in
      let next = head heirs in
      case n of
         0 -> [followers]
         n' -> (map (\x -> reverse (x : ([head (precedes g next)]))) heirs)
-}


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
beforeAndAfte r f [] = []
beforeAndAfter f (x:xs) = (x, f x) : (beforeAndAfter f xs) 


inverseMap :: [(a -> b)] -> a -> [b]
inverseMap [] x = []
inverseMap  (f:fs) x = f x : (inverseMap fs x)



-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very","very","very"],["very","very","fat"],["very","fat","cat"],
--    ["very","very"],["very","fat"]]


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
occurrences 0 r = One 
occurrences n r = Concat (occurrences (n-1) r) (r) 

-- In the code it seems a bit unintuitive that 0 occurrences returns One, 
-- However, the test seems to be correct.
-- denotation (occurrences 1 (Lit 'c')) => ["c"]
-- denotation (occurrences 0 (Lit 'c')) => [""], which is equivalent to zero occurrences of One C. .
-- However, concatenation with the empty list (Zero) yields the empty list. 



optional :: (RegEx a) -> (RegEx a)
optional r = case r of
    Zero -> Zero
    One -> One



{-f' 1 xs = 
    addTwos ++ minusFives
   where
     addTwos = map (\y -> y +  2) xs
     minusFives = map (\y -> y - 5) xs

f' 2 xs = 
    addTwos ++ minusFives
   where
    addTwos = map (\y -> y + 2) (f' 1 xs)
    minusFives = map (\y -> y -5) (f' 1 xs)
-}

--- :t lets us see it in interpretor. 

-- Can be re-written using the Go function.

