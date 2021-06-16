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

-- Examples:
ex1 = T ( C "a") <> T ( C "a") <> mempty <> T ( C "a") -- example simple type
ex1' = T ( C "a") <> T ( C "b") <> mempty <> T ( C "c") -- example simple type
ex2 = sL La ex1  -- location parametrised types
ex3 = sL Ho ex1  -- location parametrised types
ex4 = mCL ex2 ex3 -- merging location parametrised types
ex5 = mDL ex4 ex4 -- merging location parametrised types
ex6 = ((WT ex4) :=> (WT ex4)) == ((WT ex4) :=> (WT (sL Ho $ T E))) -- comparisson of types works
ex7 = let a = ((WT ex4) :=> (WT ex4)) in a == a 


{-
--------------------
Ex. 0: What types mean:

x: a :-> b :-> c :=> d :-> e
---
x: a  ∧ (b ∧ c)  |-  d ∧ e

y: * :=> a
---
y: * |- a
y: a

g: a :=> *
---
g: a |- *

h: a :=> b
---
g: a |- b

--------------------
Ex. 1.1: Composition
x: (a :-> b :-> c :=> d :-> e)
y: (e :-> d       :=> f)
---
x >>> y : ( a :-> b :-> c :=> f)

Or:
x: a ∨ b ∨ c |-  d ∧ e
y: e ∨ d     |-  f
---
x >>> y : a ∨ b ∨ c |- f)

Rewritten:
x: !A :=> !B
y: ?B :=> !C
x >>> y : !A :=> !C

--------------------
Ex. 1.2: Composition
x: (a :-> b :-> c :=> f :-> d :-> e)
y: (e :-> d       :=> g)
---
x >>> y : ( a :-> b :-> c :=> f :-> g )

Rewritten:
x: !A :=> f :-> !B
y: ?B :=> !C
x >>> y : !A :=> f :-> !C

--------------------
Ex. 1.3: Composition
x: (a :-> b :-> c :=> f :-> d :-> e)
y: (d :-> f       :=> g)
---
x >>> y : ( a :-> b :-> c :-> d :-> f :=> f :-> d :-> e :-> g )

Rewritten:
x: !A :=> !B :-> f
y: ?B :=> !C
x >>> y : !A :-> ?B :=> !B :-> f :-> !C

-}
