module TypeChecker
  (inputT)where

import Syntax
import Data.List (sort)
import Data.Semigroup

type TypeError = String

type Context = (String, Maybe T)

inputT :: T -> T
inputT []     = []
inputT (x:xs) = case x of
  K lt -> inputT xs
  K t1 :=> t2 -> K t1 : inputT xs

