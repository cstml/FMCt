{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-} -- To remember this is here 

module FMCt.TypeChecker (
    TError (..),
    Operations (..),
    freshVarTypes,
    typeCheck,
    normaliseT,
    typeCheckP,
    derive,
    fuse,
    consumes,
    consume,
    Derivation(..),
    (<.>),
    getTermType,
    splitStream,
    buildContext,
    pShow,
    pShow',
) where

--import Data.Set
import Control.Applicative
import Control.Exception
import FMCt.Parsing
import FMCt.Syntax
import FMCt.Aux.Pretty
import Text.Read (readMaybe)

-- | Typechecking Errors.
data TError
    = ErrSimple String    -- ^ A Simple, Generic Error.
    | ErrUndefT String    -- ^ An undefined Type.
    | ErrMerge String     -- ^ A merge Error.
    | ErrOverride String  -- ^ Attempting to override declared variable type.
    | ErrWrongT String    -- ^ Attemptig to use the wrong types.
    | ErrNotBinder String -- ^ Not a binder.
    | ErrConsume String   -- ^ Err arrising at consume level.
    | ErrFuse String      -- ^ Err arrising at fuse level.
    deriving Eq

instance Show TError where
    show = \case
        ErrSimple s -> "ERR! " ++ s
        ErrUndefT s -> "Undefined Type: " ++ s
        ErrMerge s -> "Cannot Merge: " ++ s
        ErrOverride s -> "Cannot Override: " ++ s
        ErrWrongT s -> "Wrong Type: " ++ s
        ErrNotBinder s -> "Wrong Binder: " ++ s
        ErrConsume s -> "Cannot Consume: " ++ s
        ErrFuse s -> "Cannot Fuse: " ++ s

instance Exception TError

getErrMsg :: TError -> String
getErrMsg = \case
    ErrSimple x -> x
    ErrUndefT x -> x --  An undefined Type.
    ErrMerge x -> x --  A merge Error.
    ErrOverride x -> x --  Attempting to override declared variable.
    ErrWrongT x -> x --  Attemptin to use the wrong types
    ErrNotBinder x -> x --  Not a Binder.
    ErrConsume x -> x
    ErrFuse x -> x

type Context = [(Vv, T)]

type Judgement = (Context, Term, T)

data Derivation
    = Star !Judgement
    | Variable    !Judgement 
    | Abstraction !Judgement !Derivation
    | Application !Judgement !Derivation
    | Fusion !Judgement !Derivation !Derivation
    deriving (Show, Eq)

type Term = Tm

--------------------------------------------------------------------------------
-- TypeCheck Function

typeCheck :: Tm -> Either TError Derivation
typeCheck = derive

-- | Typecheck Print - useful for debugging.
typeCheckP :: Tm -> IO ()
typeCheckP t = do
    res <- try $ evaluate $ (show . derive) t
    either (\x -> (putStrLn . getErrMsg) (x :: TError)) putStrLn res

getTermType :: Tm -> Either TError T
getTermType = (getDType <$>) . derive
  where
    getDType :: Derivation -> T
    getDType = (\(_, _, x) -> x) . getJudgement

--------------------------------------------------------------------------------
-- Aux Functions
getJudgement :: Derivation -> Judgement
getJudgement = \case
    Star j -> j
    Variable j -> j
    Application j _ -> j
    Abstraction j _ -> j
    Fusion j _ _ -> j

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

splitStream :: [a] -> ([a], [a])
splitStream x = (,) l r
  where
    l = snd <$> (filter (odd . fst) $ zip ([1 ..] :: [Integer]) x)
    r = snd <$> (filter (not . odd . fst) $ zip ([1 ..] :: [Integer]) x)

--------------------------------------------------------------------------------

fuseTypesD :: Derivation -> Derivation -> Either TError T
fuseTypesD dL dR = ty
  where
    tL = (getJType . getJudgement) dL :: T
    tR = (getJType . getJudgement) dR :: T
    ty = fuse tL tR

getJType :: Judgement -> T
getJType (_, _, x) = x

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

-- | Second step is to add our known variables to the context
derive :: Term -> Either TError Derivation
derive p = flat $ derive' freshVarTypes <$> (buildContext emptyCtx p) <*> pure p
  where
    emptyCtx = [("*", TCon [] :=> TCon [])]
    gCtx :: Derivation -> Context
    gCtx = \case
        Star (c, _, _) -> c
        Variable (c, _, _) -> c
        Abstraction (c, _, _) _ -> c
        Application (c, _, _) _ -> c
        Fusion (c, _, _) _ _ -> c
    getType :: Term -> Context -> Either TError T
    getType = \case
        t@(V b St) -> \case
            [] ->
                Left $
                    ErrUndefT $
                        mconcat
                            [ "Cannot Find type for binder: "
                            , show b
                            , " in context. Have you defined it prior to calling it ?"
                            ]
            ((b', ty) : xs) -> if b == b' then pure ty else getType t xs
        St -> \_ -> pure $ mempty :=> mempty
        t -> \_ ->
            Left . ErrNotBinder $
                mconcat ["Attempting to get type of:", show t]
    getUpperType :: Derivation -> T
    getUpperType = \case
        Star (_, _, t) -> t
        Variable (_, _, t) -> t
        Abstraction (_, _, t) _ -> t
        Application (_, _, t) _ -> t
        Fusion (_, _, t) _ _ -> t

    derive' :: [T] -> Context -> Term -> Either TError Derivation
    derive' stream ctx term =
        case term of
            St -> Star . (,,) ctx St <$> ty
              where
                ty = getType term ctx
            V _ St -> Variable . (,,) ctx term <$> ty
              where
                ty = getType term ctx
            V x nT -> Fusion <$> ((,,) ctx term <$> ty) <*> dLeft <*> dRight
              where
                dLeft = derive' nStreamL ctx (V x St)
                dRight = derive' nStreamR ctx nT
                ty = flat $ fuseTypesD <$> dLeft <*> dRight
                nStream = tail stream
                (nStreamL, nStreamR) = splitStream nStream
            B x ty lo St -> Abstraction (ctx', term, tT) <$> nDeriv
              where
                nDeriv = derive' nStream ctx' (V x St)
                bT = normaliseT $ ty
                tT = TLoc lo bT :=> mempty
                ctx' = (x, bT) : ctx
                nStream = tail stream
            B x ty lo nT -> Fusion <$> ((,,) ctx' term <$> ty') <*> dLeft <*> dRight
              where
                ctx' = (x, ty) : ctx
                nStream = tail stream
                (nStreamL, nStreamR) = splitStream nStream
                dLeft = derive' nStreamR ctx' nT
                dRight = derive' nStreamL ctx (B x ty lo St)
                ty' = flat $ fuseTypesD <$> dLeft <*> dRight
            P t lo St -> Application <$> ((,,) ctx term <$> ty) <*> nDeriv
              where
                uty = getUpperType <$> nDeriv
                ty = (TCon [] :=>) <$> TLoc lo <$> uty
                nStream = tail stream
                nDeriv = derive' nStream ctx t
            P t lo nT -> Fusion <$> ((,,) <$> nctx <*> (pure term) <*> ty) <*> dLeft <*> dRight
              where
                nStream = tail stream
                dLeft = derive' nStreamL ctx $ P t lo St
                dRight = derive' nStreamR ctx nT
                ty = either Left id $ fuseTypesD <$> dLeft <*> dRight
                nctx = flat $ mergeCtx <$> (gCtx <$> dLeft) <*> (gCtx <$> dRight)
                (nStreamL, nStreamR) = splitStream nStream

flat :: Either a (Either a b) -> Either a b
flat = either Left id

-- | Synnonym for fuse
(<.>) :: T -> T -> Either TError T
(<.>) = fuse

fuse :: T -> T -> Either TError T
fuse = \case
    TCon "" -> pure . normaliseT
    TVec [] -> pure . normaliseT
    x@(TCon _) -> \case
        TCon "" -> pure x
        y@(TCon _) -> pure $ TVec [x, y]
        TVec [] -> pure $ TVec [x]
        TVec (y : ys) -> flat $ fuse <$> fuse x y <*> pure (TVec ys)
        y@(TLoc _ _) -> pure $ TVec [x, y]
        y@(_ :=> _) -> either (Left . ErrFuse . show) (Right . normaliseT) $ consume y (mempty :=> x)
    xxx@(TVec xx@(x : _)) -> \case
        TCon "" -> pure xxx
        y@(TCon _) -> pure . TVec $ xx ++ [y]
        TVec [] -> pure xxx
        TVec (y : ys) -> flat $ fuse <$> fuse xxx y <*> pure (TVec ys)
        y@(TLoc _ _) -> pure $ TVec [x, y]
        y@(_ :=> _) -> either (Left . ErrFuse . show) (Right . normaliseT) $ consume y (mempty :=> xxx)
    x@(TLoc l t) -> \case
        TCon "" -> pure x
        y@(TCon _) -> pure $ TVec [x, y]
        TVec [] -> pure x
        TVec (y : ys) -> flat $ fuse <$> fuse x y <*> pure (TVec ys)
        y@(TLoc l' t') ->
            if l == l'
                then TLoc l <$> fuse t t'
                else pure $ TVec [x, y]
        y@(_ :=> _) -> either (Left . ErrFuse . show) (Right . normaliseT) $ consume y (mempty :=> x)
    x@(_ :=> _) -> \case
        TCon "" -> pure x
        TVec [] -> pure x
        y@(TCon _) -> pure $ TVec [x, y]
        TVec (y : ys) -> flat $ fuse <$> fuse x y <*> pure (TVec ys)
        y@(TLoc _ _) -> pure $ TVec [x, y]
        y@(_ :=> _) -> either (Left . ErrFuse . show) (Right . normaliseT) $ consume x y

-- | Consume
consume :: T -> T -> Either TError T
consume = \case
    t1@(tIn :=> tOut) -> \case
        t2@(tIn' :=> tOut') -> result
          where
            mErr =
                Left . ErrMerge . mconcat $
                    [ "Same Location interference: "
                    , "error could not merge "
                    , show t1
                    , " with "
                    , show t2
                    , ". Resulting Type: Remaining from Input:"
                    , show $ fst <$> intermediary
                    , ". Remaining from Output: "
                    , show $ snd <$> intermediary
                    ]

            diffLoc :: T -> T -> Bool
            diffLoc = \case
                TCon "" -> const True
                TVec [] -> const True
                x@(TCon _) -> \case
                    TCon "" -> True
                    TVec [] -> True
                    TCon _ -> False
                    TVec y -> all (diffLoc x) y
                    TLoc _ _ -> True
                    tt1 :=> tt2 -> diffLoc x tt1 && diffLoc x tt2
                xxx@(TVec xx@(x : xs)) -> \case
                    TCon "" -> True
                    TVec [] -> True
                    y@(TCon _) -> all (diffLoc y) xx
                    TVec y -> all (diffLoc x) y && all (diffLoc $ TVec xs) y
                    y@(TLoc _ _) -> all (diffLoc y) xx
                    tt1 :=> tt2 -> diffLoc xxx tt1 && diffLoc xxx tt2
                x@(TLoc l _) -> \case
                    TCon _ -> True
                    TVec [] -> True
                    TVec y -> all (diffLoc x) y
                    TLoc l' _ -> l /= l'
                    tt1 :=> tt2 -> diffLoc x tt1 && diffLoc x tt2
                x@(tt1 :=> tt2) -> \case
                    TCon "" -> True
                    y@(TCon _) -> diffLoc tt1 y && diffLoc tt2 y
                    TVec [] -> True
                    TVec y -> all (diffLoc tt1) y && all (diffLoc tt2) y
                    y@(TLoc _ _) -> diffLoc tt1 y && diffLoc tt2 y
                    tt1' :=> tt2' -> diffLoc x tt1' && diffLoc x tt2'

            intermediary :: Either TError (T, T)
            intermediary = tIn' `consumes` tOut

            result :: Either TError T
            result = case intermediary of
                Left x -> Left . ErrMerge . show $ x
                Right (TCon "", TCon "") -> pure $ tIn :=> tOut'
                Right (TCon "", TVec []) -> pure $ tIn :=> tOut'
                Right (TCon "", y) -> pure $ tIn :=> (y <> tOut')
                Right (TVec [], TCon "") -> pure $ tIn :=> tOut'
                Right (TVec [], TVec []) -> pure $ tIn :=> tOut'
                Right (TVec [], y) -> pure $ tIn :=> (y <> tOut')
                Right (x, TVec []) -> pure $ (tIn <> x) :=> tOut'
                Right (x, TCon "") -> pure $ (tIn <> x) :=> tOut'
                Right (x, y) ->
                    if diffLoc x y
                        then pure $ (tIn <> x) :=> (y <> tOut')
                        else mErr
        y ->
            Left . ErrWrongT $
                "Cannot consume types which are not of the form (* => *)"
                    ++ ". Attempted to consume: "
                    ++ show t1
                    ++ " "
                    ++ show y
                    ++ "."
    x -> \y ->
        Left . ErrWrongT $
            "Cannot consume types which are not of the form (* => *)"
                ++ ". Attempted to consume: "
                ++ show x
                ++ " "
                ++ show y
                ++ "."

consumes ::
    -- | Requires - What the term will consume
    T ->
    -- | Receives - what the term is getting
    T ->
    -- | (Left from Requires, Left from Receives)
    Either TError (T, T)
consumes = \case
    TCon "" -> pure . (,) mempty
    TVec [] -> pure . (,) mempty
    xx@(TCon x) -> \case
        TCon "" -> pure $ (,) xx mempty
        yy@(TCon y) ->
            if x == y
                then pure (mempty, mempty)
                else Left . ErrConsume . mconcat $ [show xx, " cannot consume ", show yy]
        TVec [] -> pure $ (xx, mempty)
        TVec (y : ys) -> case consumes xx y of
            Left e -> Left e
            Right (res1, left1) ->
                case consumes res1 (TVec ys) of
                    Left e -> Left e
                    Right (res2, left2) -> pure (res2, (left1 <> left2))
        yy -> pure (xx, yy)
    xx@(TVec (x : xs)) -> \case
        TCon "" -> pure (xx, mempty)
        TVec [] -> pure (xx, mempty)
        yy@(TVec _) -> case consumes x yy of
            Left e -> Left e
            Right (res1, left1) ->
                case consumes (TVec xs) left1 of
                    Left e -> Left e
                    Right (res2, left2) -> pure (res1 <> res2, left2)
        y -> case consumes x y of
            Left e -> Left e
            Right (res1, left1) ->
                case consumes (TVec xs) left1 of
                    Left e -> Left e
                    Right (res2, left2) -> pure (res1 <> res2, left2)
    x@(TLoc l t) -> \case
        TCon "" -> pure (x, mempty)
        TVec [] -> pure (x, mempty)
        y@(TLoc l' t') ->
            if l == l'
                then case consumes t t' of
                    Left e -> Left e
                    Right (res, left) ->
                        pure
                            ( normaliseT $ TLoc l res
                            , normaliseT $ TLoc l left
                            )
                else -- Otherwise they don't interact
                    pure (x, y)
        -- TLoc doesn't interact with any other type.
        y -> pure (x, y)
    x@(_ :=> _) -> \case
        y@(TCon _) -> pure (x, y)
        TVec [] -> pure (x, mempty)
        y@(TLoc _ _) -> pure (x, y)
        TVec (y : ys) ->
            case consumes x y of
                Left e -> Left e
                Right (res1, left1) ->
                    case consumes res1 (TVec ys) of
                        Left e -> Left e
                        Right (res2, left2) -> pure (res2, (left1 <> left2))
        y@(_ :=> _) ->
            let x' = normaliseT x
                y' = normaliseT y
             in if x' == y' then pure (mempty, mempty) else pure (x', y')

-- TODO: Fix

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

data Operations
    = Add
    | Subtract
    | Multiply
    | Divide
    | Modulus
    | If
    deriving (Eq, Ord)

instance Read Operations where
    readsPrec _ = \case
        "+"  -> return (Add, mempty)
        "-"  -> return (Subtract, mempty)
        "if" -> return (If, mempty)
        "^" -> return (Multiply, mempty)
        "%"  -> return (Modulus, mempty)
        "/"  -> return (Divide, mempty)
        i    -> return (error "", i)

instance Show Operations where
    show = \case
        Add      -> "+"
        Subtract -> "-"
        If       -> "if"
        Multiply -> "^"
        Modulus  -> "%" 
        Divide   -> "/" 

-- | Pre parses the Term for primitives and adds their type to the context.
buildContext :: Context -> Term -> Either TError Context
buildContext eCtx =
    let i = TCon "Int"
        b = TCon "Bool"

        to = (mempty :=>)

        opType :: Operations -> T
        opType = \case
            Add -> TVec [i, i] :=> TLoc La i
            Subtract -> TVec [i, i] :=> TLoc La i
            Multiply -> TVec [i, i] :=> TLoc La i
            Divide -> TVec [i, i] :=> TLoc La i
            Modulus -> TVec [i, i] :=> TLoc La i
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

-- Show Instance
-- Inspired by previous CW.
instance Pretty Derivation where
    pShow d = unlines (reverse strs)
      where
        (_, _, _, strs) = showD d
        showT :: T -> String
        showT = pShow
        showC :: Context -> String
        showC =
            let sCtx (x, t) = show x ++ ":" ++ showT t ++ ", "
             in \case
                    [] -> []
                    c -> (flip (++) " ") . mconcat $ sCtx <$> c
        showJ :: Judgement -> String
        showJ (cx, n, t) = mconcat $ showC cx{-"Γ "-} : "|- " : show n : " : " : showT t : []
        showL :: Int -> Int -> Int -> String
        showL l m r = mconcat $ replicate l ' ' : replicate m '-' : replicate r ' ' : []
        showD :: Derivation -> (Int, Int, Int, [String])
        showD (Star j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
        showD (Variable j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
        showD (Abstraction j d') = addrule (showJ j) (showD d')
        showD (Application j d') = addrule (showJ j) (showD d')
        showD (Fusion j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
        addrule :: String -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
        addrule x (l, m, r, xs)
            | k <= m =
                (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL l m r : xs)
            | k <= l + m + r =
                (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
            | otherwise =
                (0, k, 0, x : replicate k '-' : [replicate (- ll) ' ' ++ y ++ replicate (- rr) ' ' | y <- xs])
          where
            k = length x; i = div (m - k) 2; ll = l + i; rr = r + m - k - i
        extend :: Int -> [String] -> [String]
        extend i strs' = strs' ++ repeat (replicate i ' ')
        sidebyside :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
        sidebyside (l1, m1, r1, d1) (l2, m2, r2, d2)
            | length d1 > length d2 =
                (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip d1 (extend (l2 + m2 + r2) d2)])
            | otherwise =
                (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ " " ++ y | (x, y) <- zip (extend (l1 + m1 + r1) d1) d2])
    pShow d = unlines (reverse strs)
      where
        (_, _, _, strs) = showD d
        showT :: T -> String
        showT = pShow
        showC :: Context -> String
        showC =
            let sCtx (x, t) = show x ++ ":" ++ showT t ++ ", "
             in \case
                    [] -> []
                    c -> (flip (++) " ") . mconcat $ sCtx <$> c
        showJ :: Judgement -> String
        showJ (cx, n, t) = mconcat $ showC cx{-"Γ "-} : "|- " : show n : " : " : showT t : []
        showL :: Int -> Int -> Int -> String
        showL l m r = mconcat $ replicate l ' ' : replicate m '-' : replicate r ' ' : []
        showD :: Derivation -> (Int, Int, Int, [String])
        showD (Star j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
        showD (Variable j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
        showD (Abstraction j d') = addrule (showJ j) (showD d')
        showD (Application j d') = addrule (showJ j) (showD d')
        showD (Fusion j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
        addrule :: String -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
        addrule x (l, m, r, xs)
            | k <= m =
                (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL l m r : xs)
            | k <= l + m + r =
                (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
            | otherwise =
                (0, k, 0, x : replicate k '-' : [replicate (- ll) ' ' ++ y ++ replicate (- rr) ' ' | y <- xs])
          where
            k = length x; i = div (m - k) 2; ll = l + i; rr = r + m - k - i
        extend :: Int -> [String] -> [String]
        extend i strs' = strs' ++ repeat (replicate i ' ')
        sidebyside :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
        sidebyside (l1, m1, r1, d1) (l2, m2, r2, d2)
            | length d1 > length d2 =
                (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip d1 (extend (l2 + m2 + r2) d2)])
            | otherwise =
                (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ " " ++ y | (x, y) <- zip (extend (l1 + m1 + r1) d1) d2])

-- | Pretty Show of derivation hiding the context.
pShow' :: Derivation -> String
pShow' d = unlines (reverse strs)
  where
    (_, _, _, strs) = showD d
    showT :: T -> String
    showT = pShow
    showC :: Context -> String
    showC = const "Γ "
    showJ :: Judgement -> String
    showJ (cx, n, t) = mconcat $ showC cx : "|- " : pShow n : " : " : showT t : []
    showL :: Int -> Int -> Int -> String
    showL l m r = mconcat $ replicate l ' ' : replicate m '-' : replicate r ' ' : []
    showD :: Derivation -> (Int, Int, Int, [String])
    showD (Star j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
    showD (Variable j) = (0, k, 0, [s, showL 0 k 0]) where s = showJ j; k = length s
    showD (Abstraction j d') = addrule (showJ j) (showD d')
    showD (Application j d') = addrule (showJ j) (showD d')
    showD (Fusion j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))
    addrule :: String -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
    addrule x (l, m, r, xs)
        | k <= m =
            (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL l m r : xs)
        | k <= l + m + r =
            (ll, k, rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
        | otherwise =
            (0, k, 0, x : replicate k '-' : [replicate (- ll) ' ' ++ y ++ replicate (- rr) ' ' | y <- xs])
      where
        k = length x; i = div (m - k) 2; ll = l + i; rr = r + m - k - i
    extend :: Int -> [String] -> [String]
    extend i strs' = strs' ++ repeat (replicate i ' ')
    sidebyside :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
    sidebyside (l1, m1, r1, d1) (l2, m2, r2, d2)
        | length d1 > length d2 =
            (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ "  " ++ y | (x, y) <- zip d1 (extend (l2 + m2 + r2) d2)])
        | otherwise =
            (l1, m1 + r1 + 2 + l2 + m2, r2, [x ++ " " ++ y | (x, y) <- zip (extend (l1 + m1 + r1) d1) d2])
