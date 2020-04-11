module Assignment02 where

-- Imports just a few things that we have seen from the standard Prelude
-- module. (If there is no explicit 'import Prelude' line, then the entire
-- Prelude module is imported.)
import Prelude((+), (-), (*), (<), (>), (++), not, Bool(..), Char, undefined)

import Recursion

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

times :: Numb -> Numb -> Numb
times = \n -> (\m -> case n of
                      E -> E
                      S E -> m
                      S n' -> case m of
                            E -> E
                            S E -> n
                            S m' -> add n (times n m') )

equal :: Numb -> Numb -> Bool
equal = \n -> (\m -> case n of
                    E ->  case m of {E -> True; y -> False}
                    S n' -> case m of
                      E -> False
                      S m' -> equal m' n')

bigger :: Numb -> Numb -> Numb
bigger = \n -> (\m -> case m of
             E -> n  -- n is assumed bigger.
             S m' -> case n of
               E -> m -- if m is zero, then n is assumed bigger.
               S n' -> S ((bigger n') m')) -- if both are some number, check if n' or m' are bigger recurisvely.

count :: (a -> Bool) -> [a] -> Numb
count = \f -> (\l -> case l of
              [] -> E
              x:rest -> if (f x) then add one (count f rest) else count f rest)
              -- if applying f to x yields true, then add one ot the count. else, ignore and move on to rest of list.


remove :: (a -> Bool) -> [a] -> [a]
remove = \f  -> (\l -> case l of
           [] -> []
           x:rest -> if (f x) then remove f rest else x : remove f rest)
           -- if the function applied to x is true, carry on. if it is not true, then concatenate the value to a new list.

prefix :: Numb -> [a] -> [a]
prefix = \n -> (\l -> case n of
                    E -> [] -- if Zero, return an empty list
                    S n' -> case l of
                      [] -> [] -- if tlist is empty, return empty list.
                      x:rest -> x : prefix n' rest) -- if nonempty list, concatenate value of list and recurse on n'.


depth :: WFF -> Numb
depth = \r -> case r of
       T -> S E
       F -> S E
       Neg r -> add one (depth r)
       Conj r1 r2 -> add one (bigger (depth r1) (depth r2))
       Disj r1 r2 -> add one (bigger (depth r1) (depth r2))
