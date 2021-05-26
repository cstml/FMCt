{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax
  ( Tt(..)
  , Tm(..))
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString(..))
import Data.Void

------------------------------------------
-- M,N  = * | x : t .N | [M]a.N | a<x>. --
------------------------------------------

type Vl = (At, Tm)            -- Value
type Lo = Text               -- Location
type At = Text               -- Atom

-- | Terms
data Tm = Va Vl Tt Tm       -- Variable
        | Ap Tm Lo Tm       -- Application [M]a.N
        | Ab Tm Lo Tm       -- Abstraction a<x>.N 
        | S                 -- Star 
        deriving Eq

-- | Types
data Tt = Tb                 -- Boolean 
        | Fi                 -- Integer
        | Ts                 -- String
        | St                 -- T
        deriving Eq

-- | Show
instance Show Tm where
  show (Va (b,t) tt t' )
    | t' /= S   = mconcat [sU, ":", sT, ". ", show t']
    | otherwise = mconcat [sU, ":", sT]
    where
      sU = T.unpack b
      sT = show t;
  show (Ap t l t') = if t' /= S then mconcat ["[", show t, "]",T.unpack l, ". ", show t']
                     else mconcat ["[", show t, "]",T.unpack l ]
  show (Ab t l t') = mconcat [T.unpack l, "<", show t, ">",  ". ", show t']
  show S           = "*"
  
instance Show Tt where
  show Tb = "b"
  show Ts = "i"
  show St = "s"

-- | Examples
ex1 = Va ("x",S) Tb S              -- x:b . *
ex2 = Ap ex1 "a" ex1           -- [x:b]a.x:b
ex3 = Ab (Va ("x",S) Tb S) "b" ex2 -- b<x:b>. [x:b]a. x:b
-------------------------------------------------------------

