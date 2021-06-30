module TypeChecker
  ( typeCheck
  , TypeError(..)
  )
where

import Syntax --(Tm(..), T(..), K(..), Vv, GLT)
import Data.List (sort)
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.ST.Strict
import Parsing
import Text.Read (readMaybe)

typeCheck = undefined
{-
type VStream = [String]

varStream :: VStream
varStream = [ y : z : show x | x <- [1 ..], y <- ['a'..'z'], z <- ['a'..'z'] ]
-}
-- | TypeChecking Error.
data TypeError = SimpleEr String  -- ^ A Simple Error
               | TypeClash String -- ^ A Type Clash
               deriving Show
{-
type TypeVariable = String -- ^ Type Variable.

type Type = Either TypeVariable T -- ^ The type is either a typevariable or a type.

-- | Context represents each term with their type.
type Context = Map Tm Type

-- | TypeChecks a Term and returns either an error or a Boolean
typeCheck :: Tm -> Either TypeError Bool
typeCheck = return $ Right True

-- | Checks if a Term is already bound to a type in the context.
isTyped :: Tm -> Context -> Bool
isTyped t c =
  case c M.!? t of
    Nothing -> False
    _ -> True

emptyContext :: ST a Context
emptyContext = pure M.empty

addToContext :: Tm -> Type -> Context -> ST a Context
addToContext t ty c = pure $ M.insert t ty c

-- | Get the type of a term from the Context
getFromContext :: Tm -> Context -> ST a Type
getFromContext t c = pure $
  case c M.!? t of
    Just t -> t
    _      -> error "Cannot find term in context"

-- | Get the affected Locations
getLocation :: T -> [Lo]
getLocation [] = []
getLocation (K (T l _) : xs) = l : getLocation xs

-- | Creates the context for a term
buildContext :: Tm -> Context -> ST a Context
buildContext t c = buildContext' t c varStream 

buildContext' :: Tm -> Context -> VStream -> ST a Context
buildContext' St c s = pure c

buildContext' (V x t) c s = do
  case (readMaybe x :: Maybe Int) of
    Just y -> do
      y <- addToContext (V x St) (Right [K (T Ho []) :=> K (T La ["Int"])]) c
      buildContext' t y (tail s)
    Nothing -> do     
      y <- addToContext (V x St) (Left (head s)) c
      buildContext' t y (tail s)
  
buildContext' (P t l t') c s = do
  y <- addToContext t (Left (head s)) c
  buildContext' t' y (tail s)
  
buildContext' (B v ty l t') c s = do
  y <- addToContext (V v St) (Right ty) c
  buildContext' t' y (tail s)
  
testF :: String -> Context
testF s = runST $ do
  buildContext (parseFMC s) M.empty

testF' :: Context -> String -> Type
testF' context s = runST $ do
  term    <- getFromContext (V s St) context
  pure term

test1 = testF "1.2.3.<x:(=>(int))>"
test1' = testF' $ testF "1.2.3.<x:(=>(int))>"

inputT :: T -> T
inputT [] = []
inputT (t1 :=> t2 : xs) = t1 : inputT xs
inputT y = error $ show y ++ " is not a type I was expecting for inputT"

outputT :: T -> T
outputT [] = []
outputT (t1 :=> t2 : xs) = t2 : outputT xs
outputT y = error $ show y ++ " is not a type I was expecting for outputT"

(>+>) :: T -> T -> T
(>+>) = (++)
  
(>->) :: T -> T -> T            
t1 >-> t2 = undefined

createType :: Tm -> Context -> Context
createType St c = c
createType (V x t) c = runST $ do
  binding <- getFromContext (V x St) c
  return c
-}
