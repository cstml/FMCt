{-#LANGUAGE TemplateHaskell #-}
module FMCt.TypeChecker.Aux where

import Control.Monad (join)
import FMCt.Syntax
import FMCt.TypeChecker.Error
import Text.Read (readMaybe)
import Control.Lens
import qualified Data.Set as Set
import qualified Data.Map as Map

type Term = Tm

type TypingContext = Map.Map Vv T

data Judgement = Judgement
  { _jContext :: TypingContext
  , _jTerm ::  Term
  , _jType :: T
   } deriving (Show,Eq)

makeLenses ''Judgement

-- | A type substitution is a pair of types
data TSubs = TSubs
  { _sFrom :: T
  , _sTo :: T
  } deriving (Show,Eq)

makeLenses ''TSubs

data Operations
    = Add
    | Subtract
    | Multiply
    | Divide
    | Modulus
    | If
    deriving (Eq, Ord)

makePrisms ''Operations

instance Read Operations where
    readsPrec _ = \case
        "+" -> return (Add, mempty)
        "-" -> return (Subtract, mempty)
        "if" -> return (If, mempty)
        "^" -> return (Multiply, mempty)
        "%" -> return (Modulus, mempty)
        "/" -> return (Divide, mempty)
        i -> return (error "", i)

instance Show Operations where
    show = \case
        Add -> "+"
        Subtract -> "-"
        If -> "if"
        Multiply -> "^"
        Modulus -> "%"
        Divide -> "/"

-- | Merge contexts
mergeCtx :: TypingContext -> TypingContext -> Either TError TypingContext
mergeCtx ox oy = case  (Map.toList $ Map.intersection ox oy) of 
    [] -> pure $ Map.union ox oy
    x -> Left . ErrOverride $ "Type Conflict between: " <> show x
{-
-- | Normalise gets rid of empty Types at locations.
normaliseT :: T -> T
normaliseT t
    | t == normalisedT = normalisedT
    | otherwise = normaliseT normalisedT
  where
    normalisedT = normaliseT' t
    normaliseT' = \case
        TEmp -> mempty
        TVec [] -> mempty
        TLoc _ (TVec []) -> mempty
        TLoc _ (TEmp) -> mempty
        TLoc _ (TCon "") -> mempty
        TLoc l (TVec (x : xs)) -> TLoc l x <> (TLoc l $ TVec xs)
        TVec ([x]) -> normaliseT x
        t1 :=> t2 -> normaliseT t1 :=> normaliseT t2
        TVec (x : xs) -> normaliseT x <> (normaliseT $ TVec xs)
        x -> x -- Just to be sure it gets through.
-}

splitStream :: [a] -> ([a], [a])
splitStream x = (,) l r
  where
    l = snd <$> (filter (odd . fst) $ zip ([1 ..] :: [Integer]) x)
    r = snd <$> (filter (not . odd . fst) $ zip ([1 ..] :: [Integer]) x)

-- | Pre parses the Term for primitives and adds their type to the context.
buildContext :: TypingContext -> Term -> Either TError TypingContext
buildContext eCtx = undefined
{-
    let
      i = TCon "Int"
      b = TCon "Bool"
      to = (mempty :=>)
      opType :: Operations -> T
      opType = \case
            Add -> TVec [i, i] :=> TLoc La i
            Subtract -> TVec [i, i] :=> TLoc La i
            Multiply -> TVec [i, i] :=> TLoc La i
            Divide -> TVec [i, i] :=> TLoc La i
            Modulus -> TVec [i, i] :=> TLoc La i
            If ->
                TVec
                    [ b
                    , TLoc (Lo "if") $ TVar "ifVar1"
                    , TLoc (Lo "if") $ TVar "ifVar1"
                    ]
                    :=> TVec [TLoc La $ TVar "ifVar1"]
    in \case
            V x St -> do
                let rInt = (readMaybe x) :: Maybe Int
                let rBool = (readMaybe x) :: Maybe Bool
                let rOp = (readMaybe x) :: Maybe Operations
                let nCtx = maybe [] (const [(x, to i)]) rInt
                let nCtx' = maybe [] (const [(x, to b)]) rBool
                let nCtx'' = maybe [] ((: []) . ((,) x) . opType) rOp
                foldr (\m p -> join $ mergeCtx <$> pure m <*> p) (pure []) $
                    eCtx : nCtx : nCtx' : nCtx'' : []
            V x t' -> do
                let lCtx = buildContext eCtx (V x St)
                let rCtx = buildContext eCtx t'
                join $ mergeCtx <$> lCtx <*> rCtx
            P t _ t' -> do
                let lCtx = buildContext eCtx t
                let rCtx = buildContext eCtx t'
                join $ mergeCtx <$> lCtx <*> rCtx
            B _ _ _ t -> buildContext eCtx t
            St -> pure eCtx
-}
  
freshTypeVar :: [T]
freshTypeVar =
    (TVar . review tVariable)
        <$> [ [x] <> show y
            | y <- [1 ..] :: [Integer]
            , x <- ['a' .. 'z']
            ]

freshVarTypes :: [T]
freshVarTypes = zipWith (:=>) ls rs
  where
    (ls, rs) = splitStream freshTypeVar
