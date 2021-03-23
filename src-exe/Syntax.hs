{-# LANGUAGE OverloadedStrings #-}
module Syntax (FTyp(..), FMCT(..)) where

import Data.String (IsString(..))
import Control.Monad.State
import qualified Data.Text as T
import Pipes

data FMCT = A   T.Text            -- Atom
          | L   T.Text            -- Location 
          | V   FMCT  FMCT  FTyp  -- Variable
          | Ap  FMCT  FMCT        -- Application Term Location
          | Ab  FMCT  FMCT        -- Abstraction Term Location 
          | X   FMCT  FMCT        --
          | S                     -- Star 
          deriving (Eq)

data FTyp = FBool        -- Boolean 
          | FInt         -- Integer
          | FStr         -- String
          deriving (Eq)

instance Semigroup FMCT where
  (<>) = bR

instance Monoid FMCT where
  mempty = S

-------------------------------------------------------------
-- Show -----------------------------------------------------
instance Show FMCT where
  show (A x)     = T.unpack $ T.concat  ["_",x]
  show (L x)     = T.unpack $ T.concat  ["$",x]
  show (V b x t) = mconcat [ "(", show x, " :", show t, ")", "@", show b ]
  show (Ap t l)  = mconcat [ "[", show t, "]", show l ]
  show (Ab t l)  = mconcat [ "[", show t, "]", show l ]
  show (X  t1 t2)= mconcat [ show t1, " . ", show t2]
  show S         = "*"

instance IsString FMCT where
  fromString = A . T.pack

instance Show FTyp where
  show FBool = "bO"
  show FInt  = "iN"
  show FStr  = "sT"

-------------------------------------------------------------
-------------------------------------------------------------

bR :: FMCT -> FMCT -> FMCT  -- BetaReduce
bR S  x  = x         -- Star just reduces to whatever you give it
bR x  S  = x 
bR t1 t2 = X t1 t2   -- Any two terms reduce to an applcation  


--------------------------------------------------------------
-- Examples

ex1 = "HellO" :: FMCT
ex2 = V  "A" "3" FStr
ex3 = Ap ex2     (L "a")
ex4 = Ab "Hello" (L "b")

--------------------------------------------------------------
type Stack  a = [a] -- Stack
type Stk a = [a] -- Stack
type Mem   = (FMCT,Stk FMCT)        -- Mem

push :: FMCT -> State (Stack FMCT) ()
push a@(Ab t (L l)) = state $ \xs -> ((),a:xs)
push S              = state $ \xs -> ((),xs)
push x              = error $ err1 ++ show x

sameLoc :: FMCT -> [FMCT] -> (FMCT,[FMCT])
sameLoc tt@(Ap t l) []     = (S,[])
sameLoc tt@(Ap t l) (x@(Ab t' l') : xs)
  | l == l' = (x,xs)
                              

pop :: FMCT -> State (Stack FMCT) FMCT
pop (Ap t (L l)) = state $ \(x:xs)  -> (x,xs)

---------------------------- TO DO --------------------------
-- THink of how to evalutate this
{-
eval :: FMCT -> State (Stack FMCT) () -> State (Stack FMCT) ()
eval x st = do
  y <- st
  return y
  push x
  pop x
  push S

-- push an empty and then evaluate a push
exEval =  eval (Ab "Hello" (L "b")) (push S)
-}

-------------------------------------------------------------
exRun1 = (runState $ do 
          push $ Ab "Hello"       (L "b")
          push $ Ab "How Are you" (L "b")
          pop  $ Ap "x"           (L "b")
          push $ Ab "Soooo"       (L "b")
         ) []

--- I think using pipes for this might be a good idea !

loop :: Effect (State (Stack FMCT)) ()
loop = for (each [1..10]) $ \x ->
  (lift .  push) $ Ab ((fromString . show) x) (L $ (T.pack . show) x)
  

loopR = ( runState $ runEffect $ loop ) []

main = print "Working"

err1 = "Syntax:\n Tried to push wrong term - only abstraction can be pushed \nYou tried to push: "
