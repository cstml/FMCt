{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ( sL
  )where
import Syntax
import Data.List (sort)
import Data.Semigroup
import Data.Monoid
import qualified Data.Map  as M
import           Data.Map  (Map, (!?))

instance Semigroup (VT a) where
  VT a <> VT b = VT $ a ++ b

instance Monoid T where
  mempty = T E

--------------------------------------------------------------------------------
-- TypeError

-- | Informative Type Checkin Error
type TError = String 

--------------------------------------------------------------------------------
-- Functions

-- | Type difference 
(>\) :: T -> T -> T
t            >\ (T E)   = t
t            >\ (TV []) = t
(T E)        >\ _       = T E
(TV [])      >\ _       = T E
(T x)        >\ (T y)   = if x == y then T E else (T x)
(T x)        >\ t2      = TV [T x] >\ t2                               
t@(TV(x:xs)) >\ (TV(x':xs'))
  | x == x'   = (TV xs) >\ (TV xs')
  | otherwise = t

-- | Helper function to create simple Location Type 
sL :: Lo -> T -> L
sL l t = L $ M.fromList [(l,t)]

-- | Empty Location Tyle 
sE :: L
sE =  sL Ho (T E)

mCL :: L -> L -> L
mCL (L m) (L m') = L $ M.unionWith (<>) m m'

mDL :: L -> L -> L
mDL (L m) (L m') = L $ M.unionWith (>\) m m'

saturated :: TT -> Bool
saturated (WF t1)    = saturated t1
saturated (t :=> t') = saturated t && saturated t'
saturated (WT t1)    = pEmpty t1

(>>+) :: TT -> TT -> TT
(WT l1) >>+ (WT l2) = WT $ mCL l1 l2
(WF t1) >>+ (WF t2) = WF $ t1 >>+ t2
t1@(a :=> b) >>+ t2@(c :=> d) = if saturated (c >>- b)
                          then  a :=> (d >>+ (b >>- c))
                          else error $ "Failed to: " ++ show t1 ++ " >>+ " ++ show t2
t1@(WT l1) >>+ t2      = (WT sE :=> t1) >>+ t2
t1      >>+ t2@(WT l1) = t1 >>+ (WT sE :=> t2)
t1      >>+ t2
  | saturated t1 = t2
  | saturated t2 = t1 
  | otherwise = error $ "Cannot " ++ show t1 ++ " >>+ " ++ show t2

-- | Represents what is left of the output of the first term after the second
-- term runs
(>>-) :: TT -> TT -> TT
(WT l1)   >>- (WT l2)   = WT $ mDL l1 l2
(a :=> b) >>- (c :=> d) = (a :=> (b >>- c))
(WF a)    >>- (WF b)    = WF $ a >>- b
a         >>- b
  | saturated a = WT sE
  | saturated b = a 
  | otherwise   = error $ "Cannot " ++ show a ++ " >>- " ++ show b

helper :: TT -> TT -> TT
helper (a :=> b) (c :=> d) = (a :=> d >>+ (b >>- c))

-- | Fusion is the type equivalent of a non failing . or ; of the term.
(>>>) :: TT -> TT -> TT
(WT a :=> WT b) >>> (WT c :=> WT d)
  = tI :=> tO
  where
    tI = WT $ mCL a (mDL c b)
    tO = WT $ mCL d (mDL b d)
t1@(WT t) >>> t2        = ((WT $ sL Ho (T E)) :=> t1) >>> t2
t1        >>> t2@(WT t) = t1 >>> (WT sE :=> t2)

-- | Type composition
(>>.) :: TT -> TT -> Either TT TError
t1 >>. t2
  = case (t1, t2) of 
      ((WT a :=> WT b), (WT c :=> WT d)) -> result 
        where
          x = mDL c b    
          result = case pEmpty x of
            True  -> Left $ t1 >>> t2
            False -> Right $ "Typecheck Error: Cannot compose type " ++ show t1 ++ " with " ++ show t2
      (WT x, _) -> ((WT $ sL Ho (T E)) :=> t1) >>. t2

-- | Asert if the type is an empty
--
-- This means that there are no locations that still have unsaturated types 
pEmpty :: L -> Bool
pEmpty x = let y = getMap x in M.foldr (\x y -> y && (x == (T E))) True y

-- | Type Checker
tCheck :: TT -> TT -> TT
tCheck x y
  = case x >>. y of
      Right x -> error x
      Left  x -> x

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


--------------------------------------------------------------------------------
-- Example Composition
exc1 = ti :=> to
  where
    ti = WT $ sL Ho (T E)
    to = WT $ sL Ho (T E)

exc2 = ti :=> to'
  where
    e  = WT $ sL Ho (T E)
    ti = e
    to = WT $ sL Ho (T $ C "Int")
    to'= e :=> WF to

exc3 = exc1 >>. exc2

--------------------------------------------------------------------------------
-- Test Terms
ext1 = t1 :=> t2
  where
    t1 = WT $ sL Ho (T E)
    t2 = t1 :=> (WT $ sL Ho (T $ C "Int"))

ext2 = t1 :=> t2
  where
    t1 = WT $ sL Ho (T E)
    t2 = t1 :=> (WT $ sL La (T $ C "Int"))



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

--------------------
a a a => a a
a a   => a
---
a a a => a

--------------------
a a a => b b 
a a   => c
---
a a a (a a) => c (b b)

--------------------
A => B
D => E
---
A + D - B => E + B - D

-}
