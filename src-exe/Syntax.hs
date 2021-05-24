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

main = print "Working"
