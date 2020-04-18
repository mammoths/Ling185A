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


forward :: (Eq sy) => SLG sy -> Int -> sy -> [[sy]]
forward g 0 q = [follows g q]
forward g n q = forward' g n [q] 

forward' :: (Eq sy) => SLG sy -> Int -> [sy] -> [[sy]]   
forward' g n xs = let (start, final, trans) = g in
     let followers = follows g (head xs) in 
         case n of
             0 -> [followers]
             n' ->  ((concat (forward' g (n-1) xs)):map (\x -> (head xs) : x) [follows g (head (tail followers))])
   --          (map (\x -> reverse (x : ([head (precedes g next)]))) heirs)

         -- we wanna 
         -- followers is what follows head xs, xs 
         


{- let followers = follows g (head xs) in
     let heirs = concat [follows g (head (tail followers))] in
      let next = head heirs in
      case n of
         0 -> [followers]
         n' -> (map (\x -> reverse (x : ([head (precedes g next)]))) heirs)
-}
-- This forwards function does not give the correct output. only passes one test case. 
-- gonna need helps.

-- forward should 

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
-- plan: take followers list, prepend the preceding values of the followers list. each iteration of forwards goes deeper, and prepends more.        
 -- 0 --> ["cat", "very", fat"]
 -- 1 --> takes this list, and transforms it to be:
 --  		take each element of followers, 
 --         apply prepend, add it to each individual item in its own new list. 


-- 0 is like "what's on the menu?"
-- 1 is like.. menu + subscribers (precede)
-- 3 is like, 
    -- 1) if it's 0, return what follows that head element of xs 
    -- 2) if it's 1, return precedes? (the cat, the very, the fat)
    -- num represents distance... 
--    let next = head (follows g (fst tail xs)) in
--       case next of
--         ([] -> []
--          t -> (follows g (head xs):[(head xs):xs])
--       case n of
 --           0 -> [(head xs):[]]
 --           int' -> (follows g next): forward' g(n-1) (tail xs)
-- q: (follows g next) : forward' g(n-1) xs


-- MORE EXAMPLE USAGE:
-- forward g2 1 "the"
-- => [["the","cat"],["the","very"],["the","fat"]]
-- forward g2 2 "very"
-- => [["very","very","very"],["very","very","fat"],["very","fat","cat"],
--    ["very","very"],["very","fat"]]

-- follows gives us the list of transitionable items from x. 
-- forward gives us an iter, x, grammar, and returns
-- ret: follows list, plus follows + following THAT list. "who are my followers following."


--let (start, final, trans) = g in 
--    let int = n in 
--    let next = head (follows g q) in 
--     case int of
 --        0 -> [follows g q]
  --       int' -> (follows g next) : forward g (n-1) q


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



