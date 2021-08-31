{-# LANGUAGE ScopedTypeVariables #-}

module FMCt.Evaluator (
    Binds,
    Memory,
    State,
    emptyMem,
    eval,
    eval1,
    eval1',
    evalToString,
    tryEval1,
) where

import Control.Exception (IOException, try)
import qualified Control.Exception as E
import Data.Map (Map, (!?))
import qualified Data.Map as M
import FMCt.Syntax (Lo (..), Tm (..), Vv)
import FMCt.TypeChecker (typeCheck)
import Text.Read (readMaybe)

-- | Memory is a Map between a location and a list of Terms.
type Memory = Map Lo [Tm]

-- | Binds are a Map of Tms refering to a list of Terms.
type Binds = Map Vv [Tm]

-- | FMCt State is formed from a tuple of Memory and Binds
type State = (Memory, Binds)

-- | Pushes a term to a location the memory
push :: Tm -> Lo -> State -> State
push t In _ = error $ "Cannot push term: " ++ show t ++ " to in" -- not allowed
push t Rnd _ = error $ "Cannot push term: " ++ show t ++ " to rnd" -- not allowed
push t Nd _ = error $ "Cannot push term: " ++ show t ++ " to nd" -- not allowed
push t l (m, b) = maybe (M.insert l [t] m, b) (\x -> (M.insert l (t : x) m, b)) $ m !? l 

-- | Binds a term to a value and puts in the memory
bind :: Vv -> Lo -> State -> State
bind vv l (m, b) = case m !? l of 
    Nothing -> error $ "Empty Location " ++ show l
    Just [] -> error $ "Empty Location " ++ show l
    Just (x : xs) -> (M.insert l xs m, M.insert vv [x] b)

-- | Pops one value from a given location
pop :: Int -> Lo -> State -> State
pop n l st@(m, b)
    | n < 1 = st
    | otherwise = pop (n - 1) l nSt
  where
    nSt = case m !? l of
        Just [] -> error "Empty Stack"
        Nothing -> error "Empty Stack"
        Just (x : xs) -> push x Ho (M.insert l xs m, b)

-- | Hacky way to add numbers - TODO: refactor
adds :: String -> String -> String
adds x y = case (readMaybe x :: Maybe Int, readMaybe y :: Maybe Int) of
    (Just x', Just y') -> show $ x' + y'
    _ -> x ++ y

-- | Hacky way to add numbers - TODO: refactor
add :: State -> State
add (m, b) = t
  where
    t = case m !? Ho of
        Nothing -> error "Not enough numbers to add! Empty Stack!"
        Just [] -> error "Not enough numbers to add! Empty Stack!"
        Just [_] -> error "Not enough numbers to add!"
        Just (x : y : xs) -> case (x, y) of
            (V x' _, V y' _) -> push (V (x' `adds` y') St) Ho (M.insert Ho xs m, b)
            (_, _) -> error $ "Cannot add these terms"

evaluate :: Tm -> State -> State
evaluate (V "+" c) m = evaluate c $ add (pop 2 Ho m)
evaluate St m = m -- does nothing
evaluate (V x c) st@(_, b) = evaluate c nt -- places value @ lambda pos
  where
    nt =
        if unbound
            then push (V x St) Ho st
            else push (head et) Ho st
    et =
        maybe (error "This should have never happened - pushing unbound") id $
            b !? x
    unbound = maybe False (const True) $ b !? x
evaluate (B v _ lo tm) m = evaluate tm (bind v lo m) -- pops and binds the term
evaluate (P te l t') st = evaluate t' (push te l st) -- pushes the term

-- | The Empty FMCt Evaluator state.
emptyMem :: State
emptyMem = (,) M.empty M.empty

-- | Takes a list of terms and evaluates them one after another starting from
-- the head.
eval :: [Tm] -> State
eval t = foldl1 (flip (.)) (evaluate <$> t) emptyMem

-- | Takes 1 term and evaluates it by running it on an FMCt machine starting
-- from an empty memory.
eval1 :: Tm -> State
eval1 t = evaluate t emptyMem

-- | Evaluate with the typechecker and fail safely.
eval1' ::
    -- | Term to be evaluated.
    Tm ->
    -- | Returns an empty state if the term cannot be typed checked.
    IO State
eval1' term = do
    tc <- pure $ typeCheck term
    case tc of
        Left e -> putStrLn "Caugh Error: " >> print e >> pure emptyMem
        Right _ -> pure $ eval1 term

evalToString :: Tm -> IO String
evalToString term = do
    tc <- return $ typeCheck term
    case tc of
        Left e -> pure $ show e
        Right _ -> pure . show $ eval1 term

-- | Evaluates a term and fails safely.
tryEval1 :: Tm -> IO (Either IOException State)
tryEval1 term = do
    x :: Either IOException State <- try $ E.evaluate $ eval1 term
    return x
