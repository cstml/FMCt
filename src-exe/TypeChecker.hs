{-# LANGUAGE OverloadedStrings #-}
module TypeChecker where
import Syntax
import Data.List (sort)
import Data.Semigroup
import Data.Monoid
import qualified Data.Map  as M
import           Data.Map  (Map, (!?))


instance Semigroup T where
  (T E) <> t  = t
  t <> (T E)  = t 
  t1@(T t) <> t2@(T t') = TV $ t1 : t2 :[]
  t1@(T t) <> t2@(TV t') = TV $ t1 : t'
  t1@(TV t) <> t2@(T t') = TV $ t ++ [t2]
  t1@(TV t) <> t2@(TV t') = TV $ t ++ t'

instance Monoid T where
  mempty = T E

-- | Type difference 
(>\) :: T -> T -> T
t >\ (T E) = t
(T E) >\ _ = T E
(TV []) >\ _ = T E
t@(TV(x:xs)) >\ (TV(x':xs'))
  | x == x'   = (TV xs) >\ (TV xs')
  | otherwise = t
  

sL :: Lo -> T -> L
sL l t = L $ M.fromList [(l,t)]

mCL :: L -> L -> L
mCL (L m) (L m') = L $ M.unionWith (<>) m m'

mDL :: L -> L -> L
mDL (L m) (L m') = L $ M.unionWith (>\) m m'

-- | Fusion is the type equivalent of the . or ; of the term.
(>>>) :: TT -> TT -> TT
(WT a :=> WT b) >>> (WT c :=> WT d) = tI :=> tO
  where
    tI = WT $ mCL a (mDL c b)
    tO = WT $ mCL d (mDL b d)


-- Examples:
ex1 = T ( C "a") <> T ( C "a") <> mempty <> T ( C "a") -- example simple type
ex1' = T ( C "a") <> T ( C "b") <> mempty <> T ( C "c") -- example simple type
ex2 = sL La ex1  -- location parametrised types
ex3 = sL Ho ex1  -- location parametrised types
ex4 = mCL ex2 ex3 -- merging location parametrised types
ex5 = mDL ex4 ex4 -- merging location parametrised types
ex6 = ((WT ex4) :=> (WT ex4)) == ((WT ex4) :=> (WT (sL Ho $ T E))) -- comparisson of types works
ex7 = let a = ((WT ex4) :=> (WT ex4)) in a == a 

--------------------------------------------------------------------------------
-- Examples fusion
exf1 = t1 :=> t2
  where
    t1 = WT $ sL Ho (T E)
    t2 = WT $ sL Ho (T E)

exf2 = t1 :=> t2
  where
    t1 = WT $ sL Ho (T E)
    t2 = WT $ sL Ho (T $ C "Int")

exf3 = exf1 >>> exf2

{-
--------------------
Ex. 1.1: Fusion
x: a b c => d e
y: d e => f
---
x >>> y : a b c => f

--------------------
Ex. 1.2: Fusion
x: a b c => d e g
y: d e => f
---
x >>> y : a b c => f g 

--------------------
Ex. 1.3: Fusion
x: a b c => d e g
y: x k y => z h
---
x >>> y : a b c x k y => z h d e g

-}
