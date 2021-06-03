{-# LANGUAGE OverloadedStrings #-}

module Syntax
  ( TT(..)
  , Tm(..)
  , Vv(..)
  , Vt(..)
  , MT(..)
  , Lo(..))
where

import Data.String (IsString(..))

--------------------------------------
-- Location = {out, in, rnd, nd, x} --
--------------------------------------

data Lo = Out             -- User Output Location
        | In              -- User Input Location
        | Rnd             -- Rnd Input Stream
        | Nd              -- Non Deterministic Stream 
        | Ho              -- Default Location:    λ ∈ A
        | L  String       -- any other location:  x ∈ A
        deriving (Eq, Ord)

------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
------------------------------------------

-----------------------------------------
-- Types                               --
-- a -> b                              --
-- b -> c                              --
-- a, b -> c , b                       --
-- s,t  ::=  { ?s_a => !t_a | a in A } --
-- !t = t_1 ... t_n                    --
-- ?t = t_n ... t_1                    --
-----------------------------------------

-- | Variable Value 
type Vv = String            -- Variable Value

-- | Terms
data Tm = Va Vv Tm          -- Variable
        | Ap Tm Lo Tm       -- Application or Push [M]a.N
        | Ab Vv TT Lo Tm    -- Abstraction or Pop  a<x:t>.N 
        | St                -- Star 
        deriving (Eq)

-- | Variable types
-- | A type can be a variable or Star i.e. void
data Vt = CV String      
        | Star
        deriving (Eq, Ord)

infixl 9 :->

-- | Basic types 
data TT = CT Vt       -- a constant type 
        | VT Lo Vt    -- a location parametrised type
        | TT :-> TT   -- a vector type 

-- | 
data MT = TT :=> TT   
        deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- IsString instances
instance IsString Lo where
  fromString x = case x of
        "in"  -> In
        "out" -> Out
        "rnd" -> Rnd
        "nd"  -> Nd
        _     -> L x

instance IsString Vt where
  fromString x = case x of
    []  -> Star
    "*" -> Star
    _   -> CV x

instance IsString TT where
  fromString x = CT $ fromString x

-- Show instances
instance Show Lo where
  show x = case x of
    Out  -> "out"
    In   -> "in"
    Rnd  -> "rnd"
    Nd   -> "nd"
    Ho   -> "λ"
    L x  -> x
  
instance Show Vt where
  show x = case x of
    Star -> "*"
    CV a -> a

instance Show Tm where
  show x = case x of
    Ab v t l t' -> show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
    Ap t l t'   -> "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
    Va v t      -> v ++ "." ++ show t
    St          -> "*"

instance Show TT where
  show x = case x of
    (CT v)      -> show  v
    (VT l v)    -> show l ++ mconcat ["(", show v,")"]
    (t1 :-> t2) -> show t1 ++ " :-> " ++ show t2

-- Eq & Ord
instance Eq TT where
  (CT a)    == (CT b)      = a == b
  (VT l t)  == (VT l' t')  = l == l' && t == t'
  ti :-> to == ti' :-> to' = ti == to
  _         == _           = False

instance Ord TT where
  compare (CT a) x =
    case x of
      (CT b) -> EQ
      _      -> LT
  
  compare (VT l _) (VT l' _)
    | (l < l')  = LT
    | l == l'   = EQ
    | otherwise = GT

  compare (VT l _) (CT _) = LT
  
  compare (t1 :-> t2) (t1' :-> t2')
    | t1 == t1' && t2 == t2' = EQ
    | t1 <  t1' || t2 < t2'  = LT
    | t1 >  t1' || t2 > t2'  = GT


