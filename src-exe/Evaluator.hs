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
adds x y = case (readMaybe x :: Maybe Int, readMaybe y :: Maybe Int) of
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
evaluate (B v ty lo tm) m = evaluate tm (bind v lo m)        -- pops and binds the term
evaluate (P te l t') st@(m,b) = evaluate t' (push te l st)   -- pushes the term
evaluate (E t    t') st@(m,b) = evaluate t' (evaluate t st)  -- evaluates the term 
              
emptyMem :: State
emptyMem = (M.empty, M.empty)

-- | Takes a list of terms and evaluates them
eval :: [Term] -> State
eval t = foldl1 (flip (.)) (evaluate <$> t) emptyMem

eval1 :: Term -> State
eval1 t = evaluate t emptyMem

evalIO :: State -> IO ()
evalIO st@(m,b) = case m !? Out of
                    Just (x:xs)  -> print (show x)
                      >> evalIO (M.insert Out xs m, b)
                    Just []      -> return ()
                    Nothing      -> return ()

ex7 = eval1 $  -- [1.2.*].<x:t>.x.3.4
      (P (V "1" $ V "2" St) La)     -- [1 . 2 . *]
      (B "x" emptyT La              -- <x:t> 
        $ V "x"                     -- x
        $ V "3"                     -- 3
        $ V "4"                     -- 4
        $ V "+"
        $ V "+"
        St)
      
ex8 = eval1 $ -- 1 . 2 . <x:t>_ . x . +
      (V "1" 
       $ V "2"
       $ B "x" emptyT Ho
       $ V "x"
       $ V "+" St)

