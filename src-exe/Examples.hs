module Examples where

import Syntax 

-- Simple Terms

ex1 = (Va "a" St)
ex2 = (Va "a" ex1)
ex3 = (Ab "x" "i" "a" St)
ex4 = (Ap ex1 In St)

t2 = "a" :-> (VT "a" "3") :-> (VT "a" "2") :-> (VT "b" "5") :-> (VT "a" "1")
t1 = "a" :-> "b" :-> "c"
