{-# LANGUAGE OverloadedStrings #-}
module TypeChecker where
import Syntax

-- | takes any type and puts in the modulus form
-- | i.e. sorts by location while maintaining the order  
modulusForm :: TT -> TT
modulusForm t@(CT _)   = t
modulusForm t@(VT _ _) = t
modulusForm (t1 :-> t2)  = runner $ t1 :->  t2
  where
    runner = chainer . nrm 

sort []     = []
sort (x:xs) = (sort . smaller) x ++ x : equal x ++ (sort . larger) x
  where
    smaller x = [ y | y <- xs, compare y x == LT]
    equal   x = [ y | y <- xs, compare y x == EQ]
    larger  x = [ y | y <- xs, compare y x == GT]    

-- | Breaks down a type into a list of constant types
-- | sorted in modulus equivalent form.
nrm :: TT -> [TT]
nrm (t1 :-> t2) = sort $ (nrm t1) ++ (nrm t2)
nrm t           = [t]

-- | Chains type together into their vector modulus form.
chainer :: [TT] -> TT
chainer []     = CT Star
chainer (x:[]) = x
chainer (x:xs) = x :-> chainer xs

-- | Flips from type !a to type ?a
flipper :: TT -> TT
flipper  = chainer . reverse . nrm

machineRun :: TT -> TT -> MT
machineRun x y  =  z :=> flipper z
  where z =  modulusForm $ y :->  x

-- example typings
t2 = "a" :-> (VT "a" "3") :-> (VT "a" "2") :-> (VT "b" "5") :-> (VT "a" "1")
t1 = "a" :-> "b" :-> "c"
