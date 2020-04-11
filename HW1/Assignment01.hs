module Assignment01 where

data WFF = T | F | Neg WFF
         | Conj WFF WFF | Disj WFF WFF deriving Show

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

denotation :: WFF -> Bool
denotation = \wff -> case wff of 
                      T -> True
                      F -> False
                      Neg form -> case (denotation form) of {True -> False; False -> True} 
                      Conj form1 form2 -> case (denotation form1) of {True -> denotation form2; False -> False}
                      Disj form1 form2 -> case (denotation form1) of {True -> True; False -> denotation form2}


