{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

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

emptyM :: State
emptyM = (M.empty, M.empty)

evaluate :: Term -> State -> State
evaluate St       m = m                                -- does nothing
evaluate (Va x c) m = evaluate c (push (Va x St) Ho m) -- places value @ lambda pos
evaluate (Ab v ty lo tm) m = evaluate tm (bind v lo m) -- pops and binds the term
evaluate (Ap t l t') m = evaluate t' (push t l m)      -- pushes the term

----------------------------------------------------------
-- -- | Terms                                           --
-- data Tm = Va Vv Tm          -- Variable              --
--         | Ap Tm Lo Tm       -- Application [M]a.N    --
--         | Ab Vv TT Lo Tm    -- Abstraction a<x:t>.N  --
--         | St                 -- Star                 --
--         deriving (Eq, Show)                          --
----------------------------------------------------------

ex1 = evaluate St emptyM
ex2 = foldl1 (flip (.)) (evaluate <$> [t,t,t]) emptyM
  where
    t = (Va "c" St)
ex4 = foldl1 (flip (.)) (evaluate <$> [t1,t2,t3,t4]) emptyM
  where
    t1 = (Ap (Va "c" St) "o" St)
    t2 = (Ab "x" "int" "o" St)
    t3 = (Va "c" St)
    t4 = (Ab "z" "int" Ho St)

ex3 = foldl1 (flip (.)) (evaluate <$> [t1, t2, t2]) emptyM
  where
    t1 = (Ap (Va "c" St) "o" St)
    t2 = (Ab "x" "int" "o" St) 
