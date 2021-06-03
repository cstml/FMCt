{-# LANGUAGE OverloadedStrings #-}
module Examples where

import Evaluator
import Syntax
import Pretty 

-- Simple Terms
ex1 = (Va "a" St)
ex2 = (Va "a" ex1)
ex3 = (Ab "x" "i" "a" St)
ex4 = (Ap ex1 In St)

-- Types
t2 = "a" :-> (VT "a" "3") :-> (VT "a" "2") :-> (VT "b" "5") :-> (VT "a" "1")
t1 = "a" :-> "b" :-> "c"

-- Evaluation
eex1 = evaluate St emptyMem
eex2 = foldl1 (flip (.)) (evaluate <$> [t,t,t]) emptyMem
  where
    t = (Va "c" St)

-- Pretty Printer
peex2 = printStack eex2
