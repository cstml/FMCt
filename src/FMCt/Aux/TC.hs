module FMCt.Aux.TC where

import FMCt.Syntax
import Text.Read (readMaybe)

-- | Merge contexts
mergeCtx :: Context -> Context -> Either TError Context
mergeCtx ox oy = makeSet ox oy
  where
    makeSet [] x = pure x
    makeSet (t : xs) [] = (t :) <$> makeSet xs oy
    makeSet t@((term, ty) : xs) ((term', ty') : ys) =
        if term /= term'
            then makeSet t ys
            else
                if ty == ty'
                    then makeSet xs oy
                    else
                        Left . ErrOverride $
                            "Type Conflict between: "
                                ++ show term
                                ++ ":"
                                ++ show ty
                                ++ " and "
                                ++ show term'
                                ++ ":"
                                ++ show ty'



flat :: Either a (Either a b) -> Either a b
flat = either Left id


-- | Pre parses the Term for primitives and adds their type to the context.
buildContext :: Context -> Term -> Either TError Context
buildContext eCtx =
    let i = TCon "Int"
        b = TCon "Bool"

        to = (mempty :=>)

        opType :: Operations -> T
        opType = \case
            Add -> TVec [i, i] :=> i
            Subtract -> TVec [i, i] :=> i
            If -> TVec[ b
                      , TLoc (Lo "if") $ TVar "ifVar1"
                      , TLoc (Lo "if") $ TVar "ifVar1"
                      ] :=>
                  TVec [ TLoc La $ TVar "ifVar1"]
     in \case
            V x St -> do
                let rInt = (readMaybe x) :: Maybe Int
                let rBool = (readMaybe x) :: Maybe Bool
                let rOp = (readMaybe x) :: Maybe Operations
                let nCtx = maybe [] (const [(x, to i)]) rInt
                let nCtx' = maybe [] (const [(x, to b)]) rBool
                let nCtx'' = maybe [] ((: []) . ((,) x) . opType) rOp
                foldr (\m p -> flat $ mergeCtx <$> pure m <*> p) (pure []) $
                    eCtx : nCtx : nCtx' : nCtx'' : []
            V x t' -> do
                let lCtx = buildContext eCtx (V x St)
                let rCtx = buildContext eCtx t'
                flat $ mergeCtx <$> lCtx <*> rCtx
            P t _ t' -> do
                let lCtx = buildContext eCtx t
                let rCtx = buildContext eCtx t'
                flat $ mergeCtx <$> lCtx <*> rCtx
            B _ _ _ t -> buildContext eCtx t
            St -> pure eCtx


splitStream :: [a] -> ([a], [a])
splitStream x = (,) l r
  where
    l = snd <$> (filter (odd . fst) $ zip ([1 ..] :: [Integer]) x)
    r = snd <$> (filter (not . odd . fst) $ zip ([1 ..] :: [Integer]) x)


freshTypeVar :: [T]
freshTypeVar =
    TVar
        <$> [ mconcat $ [[x], show y]
            | y <- [1 ..] :: [Integer]
            , x <- ['a' .. 'z']
            ]

freshVarTypes :: [T]
freshVarTypes = zipWith (:=>) ls rs
  where
    (ls,rs) = splitStream freshTypeVar


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
