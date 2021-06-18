{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ()where
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

type TypeError = String

type C = (String, Maybe 

typeCheck :: [Tm] -> C -> Either T TypeError
typeCheck (x:xs)
