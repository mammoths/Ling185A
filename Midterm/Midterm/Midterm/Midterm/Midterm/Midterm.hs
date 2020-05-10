module Midterm where

import ProbSLG
import Helpers
import Data.List (nub)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Problem 1:
-------------------------------------------------------------------------------

follows :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
follows (ProbSLG (starts, ends, trans)) q =
    map (\(_,val,p) -> (val,p)) (filter (\(q', _, _) -> q' == q) trans)

-- Follows first filters out from the trans list of the ProbSLG 
-- all the values that which transition from q. 
-- THEN, we map from the three-uple to a two-uple to reduce.
-- Follows returns the list of characters that can follow q and its probabilities.

precedes :: Ord sy => ProbSLG sy -> sy -> [(sy, Double)]
precedes (ProbSLG (starts, ends, trans)) q = 
    map (\(val, _, p) -> (val, p)) (filter (\(_, q', _) -> q' == q) trans)

-- Precedes filters the trans list all the values which match q in the 2nd element of the 3-uple.
-- then, we use map to reduce the 3-uple to return just the preceding value and probability.
-- Precedes returns the list of characters than can precede q and its probability.

valP :: Ord sy => ProbSLG sy -> [sy] -> Double
valP (ProbSLG (starts, ends, trans)) str = product
    (    (map (\(_, startProb) -> startProb) starts)   ++
         (map (\(_,_,p) -> p) $  filter (\(a, b, _) -> elem (a,b) (bigrams str)) trans)  ++ 
         (map (\(_, endProb) -> endProb) ends)
    )
    
-- valP g str returns probability of str according to the PSLG g. 
-- SO, we create a list the comprises of the probabilities of the start, end, and the transitional values.
-- To obtain the probability within the trans, we create bigrams with str.
-- We extract from the 3-uple in trans to create a tuple, compare that with our bigrams list of str
-- then extract the probability. 
-- Then, we multiple all of these probabilities to obtain our final product. 

valP' :: Ord sy => ProbSLG sy -> [sy] -> Double
valP' (ProbSLG (starts, ends, trans)) str = product
    (map (\(_,_,p) -> p) $  filter (\(a, b, _) -> elem (a,b) (bigrams str)) trans) 

-- valP' is exactly like valP but without considering the start and end probabilities. 

-------------------------------------------------------------------------------
-- Problem 2:
-------------------------------------------------------------------------------


buildProbSLG :: Ord a => Corpus a -> ProbSLG a
buildProbSLG corpus = case corpus of
    [] -> ProbSLG ([], [], [])
    x:xs -> ProbSLG ([], [], [])
       


-- Returns all the states mentioned anywhere in a ProbFSA.
allStates :: (Ord st, Ord sy) => ProbFSA st sy -> [st]
allStates (ProbFSA (starts, ends, trans)) =
    nub $ concat [ map (\(q, _) -> q) starts
                 , map (\(q, _) -> q) ends
                 , map (\(q, _, _, _) -> q) trans
                 , map (\(_, _, _, q) -> q) trans
                 ]

-- Finds starting probability of a state.
initProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> Double
initProb (ProbFSA (starts, ends, trans)) q =
    sum $ map (\(_ , p) -> p) $ filter (\(q', _) -> q' == q) starts

-- Finds ending probability of a state.
finProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> Double
finProb (ProbFSA (starts, ends, trans)) q =
    sum $ map (\(_, p) -> p) $ filter (\(q', _) -> q' == q) ends

-- Finds probability for particular state-symbol-state transition.
trProb :: (Ord st, Ord sy) => ProbFSA st sy -> st -> sy -> st -> Double
trProb (ProbFSA (starts, ends, trans)) q1 x q2 =
    sum $ map (\(_, _, p, _) -> p)
        $ filter (\(q, y, _, q') -> (q, y, q') == (q1, x, q2)) trans



newtype ProbFSA st sy = ProbFSA ( [(st, Double)]          -- Initial states.
                                , [(st, Double)]          -- Final states.
                                , [(st, sy, Double, st)]  -- Transitions.
                                )
                      deriving Show



-------------------------------------------------------------------------------
-- Problem 3:
-------------------------------------------------------------------------------

-- Add your sanitization functions to this list. Note that each function must
-- operate over sentences of tagged words.
sanitize :: [Sentence TaggedWord -> Sentence TaggedWord]
sanitize = []

posProbSLG :: Corpus TaggedWord -> ProbSLG String
posProbSLG = undefined

tag :: Corpus TaggedWord -> String -> [(Sentence TaggedWord, Double)]
tag = undefined

tagBest :: Corpus TaggedWord -> String -> Sentence TaggedWord
tagBest = undefined


