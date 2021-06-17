{-# LANGUAGE OverloadedStrings #-}
module Evaluator
  ( eval
  , State
  , emptyMem
  , evaluate
  ) where

import Syntax
import TypeChecker
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Char
import Data.String (IsString(..))
import Text.Read

type Term   = Tm
type Memory = Map Lo [Term]
type Binds  = Map Vv [Term]
type State  = (Memory, Binds)
  
-- | Pushes a term to a location the memory
push :: Term -> Lo -> State -> State
push t In  _ = error $ "Cannot push term: " ++ show t ++ " to in"   -- not allowed
push t Rnd _ = error $ "Cannot push term: " ++ show t ++ " to rnd"  -- not allowed
push t Nd  _ = error $ "Cannot push term: " ++ show t ++ " to nd"   -- not allowed
push t l (m,b) = case m !? l of
               Nothing -> (M.insert l [t]   m, b)
               Just x  -> (M.insert l (t:x) m, b)

-- | Pops a term from λ location to a given location
pushL :: Lo -> State -> State
pushL l st@(m,b) = case m !? Ho of
                     Nothing     -> error "Empty Stack when attempting to pop λ location"                    
                     Just (x:xs) -> push x l (M.insert Ho xs m, b)
                     Just _      -> error "Empty Stack when attempting to pop λ location"

-- | Binds a term to a value and puts in the memory
bind :: Vv -> Lo -> State -> State
bind vv l st@(m,b) = case m !? l of
                   Nothing      -> error $ "Empty Location " ++ show l
                   Just []      -> error $ "Empty Location " ++ show l
                   Just (x:xs)  -> (M.insert l xs m, M.insert vv [x] b)

-- | Pops one value from a given location
pop :: Int -> Lo -> State -> State
pop n l st@(m,b)
  | n < 1     = st
  | otherwise = pop (n - 1) l nSt
    where
      nSt = case m !? l of
              Just []     -> error "Empty Stack"
              Nothing     -> error "Empty Stack"
              Just (x:xs) -> push x Ho (M.insert l xs m, b)

-- | Hacky way to add numbers - TODO: refactor
adds :: String -> String -> String
adds x y = case ((readMaybe x) :: Maybe Int, (readMaybe y) :: Maybe Int) of
             (Just x, Just y) -> show $ x + y
             _                -> x ++ y

add :: State -> State
add st@(m,b) = t
  where
    t = case m !? Ho of
      Just []       -> error "Empty Stack"
      Just (x:y:xs) -> case (x, y) of
                         (V x _ , V y _) -> push (V (x `adds` y) St) Ho (M.insert Ho xs m, b)
        
evaluate :: Term -> State -> State
evaluate (V "+" c) m = evaluate c $ add (pop 2 Ho m)
  where
    intermediary = add $ pop 2 Ho m
    
evaluate St         m = m                  -- does nothing
evaluate (V x c) st@(m,b) = evaluate c nt -- places value @ lambda pos
  where
    nt = if unbound then push (V x St) Ho st
         else push (head et) Ho st
    et = case b !? x of
           Just x -> x
           _      -> error "This should have never happened - pushing unbound"
    unbound = case b !? x of
                Nothing -> True
                _       -> False                
evaluate (B  v ty lo tm) m = evaluate tm (bind v lo m)            -- pops and binds the term
evaluate (P (V x c) l t') st@(m,b) = evaluate t' (push te l st) -- pushes the bound term
  where
    te = case b !? x of
      Nothing -> St
      Just x  -> head x      
evaluate (P te l t') st = evaluate t' (push te l st)       -- pushes the term as is
              
emptyMem :: State
emptyMem = (M.empty, M.empty)

-- | Takes a list of terms and evaluates them
eval :: [Term] -> State
eval t = foldl1 (flip (.)) (evaluate <$> t) emptyMem

evalIO :: State -> IO ()
evalIO st@(m,b) = case m !? Out of
                    Just (x:xs)  -> putStrLn (show x) >> evalIO (M.insert Out xs m, b)
                    Just []      -> return ()
                    Nothing      -> return ()

ex1 = evaluate St emptyMem

ex2 = foldl1 (flip (.)) (evaluate <$> [t,t,t]) emptyMem
  where
    t = (V "c" St)
    
ex4 = foldl1 (flip (.)) (evaluate <$> [t1,t2,t3,t4]) emptyMem
   where
     t1 = (P (V "c" St) (Lo "o") St)
     t2 = (B  "x"  (WT (sL Ho (T $ C "int"))) (Lo "o") St)
     t3 = (V "c" St)
     t4 = (B  "z" (WT (sL Ho (T $ C "int"))) Ho St)

-- ex3 = foldl1 (flip (.)) (evaluate <$> [t1, t2, t2]) emptyMem
--   where
--     t1 = (P (V "c" St) "o" St)
--     t2 = (B  "x" "int" "o" St) 

-- ex5 = eval [t3, t2, t5, t6]
--   where
--     t1 = (P "c" "o" St)
--     t2 = (B  "x" "int" "" St)
--     t3 = (V "c" St)
--     t4 = (B  "z" "int" Ho St)
--     t5 = (V "c" St)
--     t6 = (P "x" "out" St)

-- ex6 = evalIO $ eval [V "2" $ V "232132" $ V "+" $ B "out" "Int" Ho $ P "out" "out" $ St ]
