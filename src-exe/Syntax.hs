{-# LANGUAGE OverloadedStrings #-}

module Syntax
  ( TT(..)
  , Tm(..)
  , Vv(..)
  , Lo(..))
where

import Data.String (IsString(..))
import Data.List   (sort)

--------------------------------------
-- Location = {out, in, rnd, nd, x} --
--------------------------------------

data Lo = Out             -- Location
        | In
        | Rnd
        | Nd
        | Ho              -- Default Location
        | L  String         
        deriving (Eq, Ord)

instance Show Lo where
  show Out   = "out"
  show In    = "in"
  show Rnd   = "rnd"
  show Nd    = "nd"
  show Ho    = "λ"
  show (L x) = x

------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
------------------------------------------

type Vv = String            -- Variable Value
    

-- | Terms
data Tm = Va Vv Tm          -- Variable
        | Ap Tm Lo Tm       -- Application [M]a.N
        | Ab Vv TT Lo Tm    -- Abstraction a<x:t>.N 
        | St                 -- Star 
        deriving (Eq)

-----------------------------------------
-- Types                               --
-- a -> b                              --
-- b -> c                              --
-- a, b -> c , b                       --
-- s,t  ::=  { ?s_a => !t_a | a in A } --
-- !t = t_1 ... t_n                    --
-- ?t = t_n ... t_1                    --
-----------------------------------------

infixl 9 :->

data Vt = CV String         -- A type can be a variable or Star i.e. void
        | Star
        deriving (Eq, Ord)

data TT = CT Vt
        | VT Lo Vt
        | TT :-> TT

data MT = TT :=> TT
        deriving (Eq, Ord, Show)

instance IsString Lo where
  fromString "in"  = In
  fromString "out" = Out
  fromString "rnd" = Rnd
  fromString "nd"  = Nd
  fromString x     = L x

instance Show Vt where
  show Star   = "*"
  show (CV a) = a

instance IsString Vt where
  fromString []   = Star
  fromString "*"  = Star
  fromString x    = CV x

instance Show Tm where
  show St = "*"
  show (Ab v t l t') = show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
  show (Ap t l t')   = "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
  show (Va v t)      =  v ++ "." ++ show t

instance IsString TT where
  fromString x = CT $ fromString x

instance Show TT where
  show (CT v)      = (show ) v
  show (VT l v)    = show l ++ mconcat ["(", show v,")"]
  show (t1 :-> t2) = show t1 ++ " :-> " ++ show t2
      
instance Eq TT where
  (CT a)     == (CT b)      = a == b
  (VT l t)   == (VT l' t')  = l == l' && t == t'
  ti :-> to  == ti' :-> to' = ti == to

instance Ord TT where
  compare (CT a) (CT b)      = EQ
  
  compare (VT l _) (VT l' _)
    | (l < l')  = LT
    | l == l'   = EQ
    | otherwise = GT
    
  compare (CT _) (VT _ _ )   = EQ
  
  compare (CT _) (_ :-> _)   = EQ
  
  compare (t1 :-> t2) (t1' :-> t2')
    | t1 == t1' && t2 == t2'  = EQ
    | t1 <  t1' || t2 < t2'   = LT
    | otherwise               = GT

  compare x y = compare y x
  
modulusForm :: TT -> TT
modulusForm t@(CT _)   = t
modulusForm t@(VT _ _) = t
modulusForm (t1 :-> t2)  = runner $ t1 :->  t2
  where
    runner = chainer . nrm 

nrm :: TT -> [TT]
nrm (t1 :-> t2) = sort $ (nrm t1) ++ (nrm t2)
nrm t           = [t]

chainer :: [TT] -> TT
chainer []     = CT Star
chainer (x:[]) = x
chainer (x:xs) = x :-> chainer xs

flipper :: TT -> TT
flipper  = chainer . reverse . nrm

machineRun :: TT -> TT -> MT
machineRun x y  =  z :=> flipper z
  where z =  modulusForm $ y :->  x

t2 = "a" :-> (VT "a" "3") :-> (VT "a" "2") :-> (VT "b" "5") :-> (VT "a" "1")
t1 = "a" :-> "b" :-> "c"
