data Shape = Rock | Paper | Scissors deriving Show
n = 1
f = \ s -> case s of 
    Rock -> 334
    Paper -> 138
    Scissors -> 99
g = \ z -> z + 4
whatItBeats = \ s -> case s of 
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper
