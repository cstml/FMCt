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

sPush :: [Term] -> Lo -> State -> State
sPush s l (m,b) = (M.insert l s m, b)

pop :: Lo -> State -> State
pop l st@(m,b) = case (m !? l) of
            Nothing     -> push St Ho st -- or maybe this should be an error?
            Just (x:xs) -> sPush xs l st

bind :: Vv -> Lo -> State -> State
bind vv l st@(m,b) = case (m !? l) of
                   Nothing      -> error $ "Empty Location " ++ show l
                   Just []      -> error $ "Empty Location " ++ show l
                   Just (x:xs)  -> (M.insert l xs m, M.insert vv [x] b)

emptyM :: State
emptyM = (M.empty, M.empty)

evaluate :: Term -> State -> State
evaluate St       m = m
evaluate (Va x c) m = evaluate c (push (Va x St) Ho m)
evaluate (Ab v ty lo tm) m = evaluate tm (bind v lo m)
evaluate (Ap t l t') m = evaluate t' (push t l m)

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
ex3 = foldl1 (flip (.)) (evaluate <$> [t1, t2, t2]) emptyM
  where
    t1 = (Ap (Va "c" St) "o" St)
    t2 = (Ab "x" "int" "o" St) 
