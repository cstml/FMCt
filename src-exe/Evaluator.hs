{-# LANGUAGE OverloadedStrings #-}
module Evaluator
  ( eval
  , State
  , emptyMem
  , evaluate
  ) where

import Syntax
import Data.Map (Map, (!?))
import qualified Data.Map as M

type Term = Tm
type Memory = Map Lo [Term]
type Binds  = Map Vv [Term]
type State  = (Memory, Binds)

push :: Term -> Lo -> State -> State
push t In _ = error $ "Cannot push term: " ++ show t ++ " to in"
push t Rnd _ = error $ "Cannot push term: " ++ show t ++ " to rnd"
push t Nd _ = error $ "Cannot push term: " ++ show t ++ " to nd"
push t l (m,b) = case (m !? l) of
               Nothing -> (M.insert l [t]   m, b)
               Just x  -> (M.insert l (t:x) m, b)

bind :: Vv -> Lo -> State -> State
bind vv l st@(m,b) = case (m !? l) of
                   Nothing      -> error $ "Empty Location " ++ show l
                   Just []      -> error $ "Empty Location " ++ show l
                   Just (x:xs)  -> (M.insert l xs m, M.insert vv [x] b)

emptyMem :: State
emptyMem = (M.empty, M.empty)

evaluate :: Term -> State -> State
evaluate St       m = m                                -- does nothing
evaluate (Va x c) st@(m,b) = evaluate c nt -- places value @ lambda pos
  where
    nt = if unbound then (push (Va x St) Ho st)
         else (push (head et) Ho st)
    et = case b !? x of
           Just x -> x
    unbound = case b !? x of
                Nothing -> True
                _       -> False
evaluate (Ab v ty lo tm) m = evaluate tm (bind v lo m)            -- pops and binds the term
evaluate (Ap (Va x c) l t') st@(m,b) = evaluate t' (push te l st) -- pushes the bound term
  where
    te = case b !? x of
      Nothing -> St
      Just x  -> head x 
evaluate (Ap te l t') st = evaluate t' (push te l st)       -- pushes the term as is

-- | Takes a list of terms and evaluates them
eval :: [Term] -> State
eval t = foldl1 (flip (.)) (evaluate <$> t) emptyMem

ex1 = evaluate St emptyMem

ex2 = foldl1 (flip (.)) (evaluate <$> [t,t,t]) emptyMem
  where
    t = (Va "c" St)
    
ex4 = foldl1 (flip (.)) (evaluate <$> [t1,t2,t3,t4]) emptyMem
  where
    t1 = (Ap (Va "c" St) "o" St)
    t2 = (Ab "x" "int" "o" St)
    t3 = (Va "c" St)
    t4 = (Ab "z" "int" Ho St)

ex3 = foldl1 (flip (.)) (evaluate <$> [t1, t2, t2]) emptyMem
  where
    t1 = (Ap (Va "c" St) "o" St)
    t2 = (Ab "x" "int" "o" St) 

ex5 = eval [t3, t2, t5, t6]
  where
    t1 = (Ap "c" "o" St)
    t2 = (Ab "x" "int" "" St)
    t3 = (Va "c" St)
    t4 = (Ab "z" "int" Ho St)
    t5 = (Va "c" St)
    t6 = (Ap "x" "out" St)
