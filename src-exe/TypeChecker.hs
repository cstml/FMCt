{-# LANGUAGE OverloadedStrings #-}
module TypeChecker
  ()where
import Syntax
import Data.List (sort)
import Data.Semigroup
import Data.Monoid
import qualified Data.Map  as M
import           Data.Map  (Map, (!?))
import qualified Data.Map  as M
import           Data.List (sort)
import           Data.Monoid
import           Data.Semigroup
import           Syntax


type TypeError = String

type C = (String, Maybe TT)

typeCheck :: Tm -> C -> Either TT TypeError
typeCheck = undefined
