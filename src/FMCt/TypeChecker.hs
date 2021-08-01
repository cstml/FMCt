{-#LANGUAGE TypeSynonymInstances#-}
{-#LANGUAGE FlexibleInstances#-}
module FMCt.TypeChecker
  ( TError(..)
  , typeCheck
  , derive
  , fuse
  , consumes
  , consume
  , Derivation
  )
where

import FMCt.Syntax
import Control.Exception
import Text.Read (readMaybe)

-- | Typechecking Errors.
data TError
  = ErrSimple   String          -- ^ A Simple Error.
  | ErrUndefT   String          -- ^ An undefined Type.
  | ErrMerge    String          -- ^ A merge Error.
  | ErrOverride String          -- ^ Attempting to override declared variable.
  | ErrWrongT   String          -- ^ Attemptin to use the wrong types
  deriving Show

instance Exception TError

type Context = [(Term, T)]
type Judgement = (Context, Term, T)

data Derivation
  = Star        !Judgement
  | Variable    !Judgement
  | Abstraction !Judgement !Derivation
  | Application !Judgement !Derivation
  | Fusion      !Judgement !Derivation !Derivation

type Term = Tm

--------------------------------------------------------------------------------
-- TypeCheck Function

typeCheck :: Tm -> Derivation
typeCheck = derive

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
freshTypeVar = TCon <$> [ mconcat $ [[x],[z],show y]
                        | y <- [1..] :: [Integer]
                        , x <- ['A'..'Z']
                        , z <- ['A'..'Z']
                        ]

freshVarTypes :: [T]
freshVarTypes =  TVec . (:[]) <$> freshTypeVar

splitStream :: [a] -> ([a],[a])
splitStream x = (,) l r
  where
    l = snd <$> (filter ( odd . fst ) $ zip ([1..] :: [Integer]) x)
    r = snd <$> (filter ( not . odd . fst ) $ zip ([1..] :: [Integer]) x)              

--------------------------------------------------------------------------------
--

fuseTypesD :: Derivation -> Derivation -> T
fuseTypesD dL dR = ty 
  where
    tL = (getJType . getJudgement) dL :: T 
    tR = (getJType . getJudgement) dR :: T 
    ty = fuse tL tR 

getJType :: Judgement -> T
getJType (_,_,x) = x

-- | Merge contexts
mergeCtx :: Context -> Context -> Context
mergeCtx ox oy = makeSet ox oy
  where
    makeSet [] x  = x
    makeSet (t:xs) [] = t : makeSet xs oy 
    makeSet t@((term,ty):xs) ((term',ty'):ys)
      = if term /= term' then makeSet t ys
        else if ty == ty' then makeSet xs oy
             else throw $ ErrOverride $ "Type Conflict between: "
                  ++ show term ++ ":" ++ show ty ++ " and "
                  ++ show term' ++ ":" ++ show ty'


-- | Second step is to add our known variables to the context
derive :: Term -> Derivation
derive p = derive' freshVarTypes (buildContext emptyCtx p) p
  where

    emptyCtx = [(St, TCon [] :=> TCon [])]
    
    -- | Get the context 
    gCtx :: Derivation -> Context
    gCtx = \case
      Star        (c,_,_)     -> c
      Variable    (c,_,_)     -> c
      Abstraction (c,_,_) _   -> c
      Application (c,_,_) _   -> c
      Fusion      (c,_,_) _ _ -> c

    -- | Get type of term from Context.
    getType :: Term -> Context -> T
    getType t [] = throw $
      ErrUndefT $ mconcat ["Cannot Find type for term: ", show t
                          , " in context. Have you defined it prior to calling it ?"]
                         
    getType t ((t',ty):xs) = if t == t' then ty else getType t xs

    -- | Get the type from a Derivation
    getUpperType :: Derivation -> T
    getUpperType = \case 
      Star        (_,_,t)     -> t 
      Variable    (_,_,t)     -> t 
      Abstraction (_,_,t) _   -> t
      Application (_,_,t) _   -> t
      Fusion      (_,_,t) _ _ -> t        
    
    derive' :: [T] -> Context -> Term -> Derivation
    derive' stream ctx term 
      = case term of  
          St -> Star (ctx, St, ty)
            where
              ty = getType term ctx

          V _ St -> Variable (ctx, term, ty)
            where
              ty = getType term ctx

          V x t  -> Fusion (ctx, term, ty) dLeft dRight
            where
              ty                  = fuseTypesD dLeft dRight
              dLeft               = derive' nStreamL ctx (V x St)
              dRight              = derive' nStreamR ctx t
              nStream             = tail stream
              (nStreamL,nStreamR) = splitStream nStream
              
          B x t lo St -> Abstraction (ctx', term, ty) nDeriv
            where              
              ty      = TLoc lo t :=> TCon []
              nStream = tail stream
              nDeriv  = derive' nStream ctx' (V x St)
              ctx'    = (V x St, t) : ctx 
              
          B x t lo t' -> Fusion (ctx', term, ty) dLeft dRight
            where
              dLeft    = derive' nStreamL ctx (B x t lo St)
              dRight   = derive' nStreamR ctx' t'
              ctx'     = (V x St, t) : ctx
              ty       = fuseTypesD dLeft dRight
              nStream  = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St -> Application (ctx, term, ty) nDeriv
            where
              uty = getUpperType nDeriv
              ty = TCon [] :=> TLoc lo uty
              nDeriv = derive' nStream ctx t
              nStream = tail stream

          P t lo t' -> Fusion (nctx, term, ty) dLeft dRight
            where
              dLeft = derive' nStreamL ctx (P t lo St)
              dRight = derive' nStreamR ctx t'
              ty = fuseTypesD dLeft dRight
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream
              nctx =  mergeCtx (gCtx dLeft) (gCtx dRight)                  

fuse :: T -> T -> T
fuse = \case
  TCon "" -> normaliseT

  TVec [] -> normaliseT
  
  x@(TCon _)  -> \case
    TCon ""          ->  x
    y@(TCon _)       -> TVec [x,y]
    TVec []          -> TVec [x]
    TVec (y:ys)      -> fuse (fuse x y) (TVec ys)
    y@(TLoc _ _)     -> TVec [x,y]
    y@(_ :=> _)    -> either (throw . ErrWrongT) normaliseT $ consume y (mempty :=> x)
    
  xxx@(TVec xx@(x:_))  -> \case
    TCon ""          -> xxx
    y@(TCon _)       -> TVec $ xx ++ [y]
    TVec []          -> xxx
    TVec (y:ys)      -> fuse (fuse xxx y) (TVec ys)
    y@(TLoc _ _)     -> TVec [x,y]
    y@(_ :=> _)      -> either (throw . ErrWrongT) normaliseT $ consume y (mempty :=> xxx)
    
  x@(TLoc l t) -> \case
    TCon ""          -> x
    y@(TCon _)       -> TVec $ x : y : []
    TVec []          -> x
    TVec (y:ys)      -> fuse (fuse x y) (TVec ys)
    y@(TLoc l' t')   -> if l == l'
                        then TLoc l $ fuse t t'
                        else TVec [x,y]
    y@(_ :=> _)    -> either (throw . ErrWrongT) normaliseT $ consume y (mempty :=> x)

  x@(_ :=> _)  -> \case
    TCon ""          -> x
    y@(TCon _)       -> fuse (mempty :=> y) x
    TVec []          -> x
    TVec (y:ys)      -> fuse (fuse x y) (TVec ys)
    y@(TLoc _ _)     -> fuse (mempty :=> y) x
    y@(_ :=> _)      -> either (throw . ErrWrongT) normaliseT $ consume x y

consumes :: T
          -- ^ Requires - What the term will consume
          -> T
          -- ^ Receives - what the term is getting 
          -> (T,T)
          -- ^ (Left from Requires, Left from Receives)
consumes = \case
  TCon "" -> (,) mempty . id

  TVec [] -> (,) mempty . id

  xx@(TCon x)  -> \case
    TCon "" -> (,) xx mempty
    yy@(TCon y) -> if x == y then (,) mempty mempty
                   else throw $ ErrMerge $
                          mconcat [ "cannot merge ", show xx , " " , show yy ]
    TVec []     -> (,) xx mempty
    TVec (y:ys) ->
      let
        (res1, left1) = consumes xx y
        (res2, left2) = consumes res1 (TVec ys)
      in (,) res2 (left1 <> left2)        
    yy          -> (xx,yy)

  xxx@(TVec (x:xs)) -> \case
    TCon ""     -> (,) xxx mempty
    TVec []     -> (,) xxx mempty
    TVec (y:ys) ->
      let
        (res1,left1) = consumes xxx y
        (res2,left2) = consumes res1 (TVec ys)
      in
        (,) res2 (left1 <> left2)
    y -> let
        (res1,left1) = consumes x y
        (res2,left2) = consumes (TVec xs) left1
      in
        (,) (res1 <> res2) (left2)

  x@(TLoc l t) -> \case
    TCon "" -> (,) x mempty
    TVec [] -> (,) x mempty
    y@(TLoc l' t') ->
      -- If the location are the same we need to check what happens with the
      -- interaction between the types.
      if l == l'
      then
        let              
          (res, left) = consumes t t'                       
                     
          left' = if left == mempty || left == TVec []
                  then mempty
                  else TLoc l left
        in
          (,) res left'

        -- Otherwise they don't interact 
      else (,) x y

    -- TLoc doesn't interact with any other type.
    y       -> (,) x y

  x@(_ :=> _) ->
    \y ->
      if x == y
      then (,) mempty mempty
      else (,) x y 

-- | Normalise gets rid of empty Types at locations.
normaliseT :: T -> T
normaliseT t
  | t == normalisedT = normalisedT
  | otherwise        = normaliseT normalisedT
  where
    
    normalisedT = normaliseT' t
    
    normaliseT' = \case
      TVec []           -> mempty
      TLoc _ (TVec [])  -> mempty
      TLoc _ (TCon "")  -> mempty
      TLoc l (TVec (x:xs)) -> TLoc l x <> (TLoc l $ TVec xs)
      TVec ([x])        -> normaliseT x
      t1 :=> t2         -> normaliseT t1 :=> normaliseT t2
      TVec (x:xs)       -> normaliseT x <> (normaliseT $ TVec xs)
      x -> x -- Just to be sure it gets through.

-- | Consume                                 
consume :: T -> T -> Either String T
consume t1@(tIn :=> tOut) t2@(tIn' :=> tOut') = result  
  where
   intermediary :: (T,T)
   intermediary = tIn' `consumes` tOut

   result :: Either String T
   result = case intermediary of
     (TCon "", TCon "") -> Right $ tIn :=> tOut'
     (TCon "", TVec []) -> Right $ tIn :=> tOut'
     (TCon "", x)       -> Right $ tIn :=> (x <> tOut')
     (TVec [], TCon "") -> Right $ tIn :=> tOut'
     (TVec [], TVec []) -> Right $ tIn :=> tOut'
     (TVec [], x)       -> Right $ tIn :=> (x <> tOut')
     
     (x       ,TVec []) -> Right $ (tIn<>x) :=> tOut'
     (x       ,TCon "") -> Right $ (tIn<>x) :=> tOut'
     _ -> Left $ mconcat $
            [ "error could not merge "
            , show t1
            , " with "
            , show t2
            , ". Resulting Type: "
            , show intermediary
            ]
consume _ _ = (throw . ErrWrongT) $ "Cannot consume types which are not of the form (* => *)"

data Operations = Add
                | Subtract
                | If
                 deriving (Eq, Ord, Show)

instance Read Operations where
  readsPrec _ = \case
    "+"  -> return (Add, mempty)
    "-"  -> return (Subtract, mempty)
    "if" -> return (If,mempty)
    i    -> return (error "", i)


-- | Pre parses the Term for primitives and adds their type to the context.
buildContext :: Context -> Term -> Context
buildContext eCtx =
  let
    opType :: Operations -> T
    opType = \case
      Add      -> TVec [TCon "Int", TCon "Int"] :=> TCon "Int"
      Subtract -> TVec [TCon "Int", TCon "Int"] :=> TCon "Int"
      If       -> error "Not yet implemented!"
  in \case
    term@(V x St) -> do 
      let int    = (readMaybe x) :: Maybe Int
      let bool   = (readMaybe x) :: Maybe Bool
      let op     = (readMaybe x) :: Maybe Operations
      let nCtx   = maybe [] (const [(term,mempty :=> TCon "Int")]) int
      let nCtx'  = maybe [] (const [(term,mempty :=> TCon "Bool")]) bool
      let nCtx'' = maybe [] ((:[]).((,) term) . opType ) op 
      foldr1 mergeCtx $ eCtx : nCtx : nCtx' : nCtx'' : []

    V x t' -> do
      let lCtx = buildContext eCtx (V x St)
      let rCtx = buildContext eCtx t'
      mergeCtx lCtx rCtx

    P t _ t' -> do
      let lCtx = buildContext eCtx t
      let rCtx = buildContext eCtx t'
      mergeCtx lCtx rCtx      

    B _ _ _ t -> buildContext eCtx t
      
    St -> eCtx
    
-- Show Instance
-- Inspired by previous CW.
instance Show Derivation where
  show d = unlines (reverse strs)
    where      
      (_, _, _, strs) = showD d
      showT :: T -> String
      showT = show 
        
      showC :: Context -> String
      showC = let sCtx (x,t) =  show x ++ ":" ++ showT t ++ ", " in \case
        [] -> []
        c  -> (flip (++) " ") . mconcat $ sCtx <$> c

      showJ :: Judgement -> String
      showJ (cx,n,t) = mconcat $ showC cx : "|- " : show n : " : " : showT t : []

      showL :: Int -> Int -> Int -> String
      showL l m r = mconcat $ replicate l ' ' : replicate m '-' : replicate r ' ' : []
      
      showD :: Derivation -> (Int,Int,Int,[String])
      showD (Star j) = (0,k,0,[s,showL 0 k 0]) where s = showJ j; k = length s
      showD (Variable j) = (0,k,0,[s,showL 0 k 0]) where s = showJ j; k = length s
      showD (Abstraction j d') = addrule (showJ j) (showD d')
      showD (Application j d') = addrule (showJ j) (showD d')
      showD (Fusion j d' e) = addrule (showJ j) (sidebyside (showD d') (showD e))

      addrule :: String -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
      addrule x (l,m,r,xs)
        | k <= m
        = (ll,k,rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL  l m r : xs)

        | k <= l+m+r
        = ( ll , k , rr , (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs )

        | otherwise
        = ( 0 , k , 0 , x : replicate k '-' : [ replicate (-ll) ' ' ++ y ++ replicate (-rr) ' ' | y <- xs ] )
        where k = length x; i = div (m - k) 2; ll = l+i; rr = r+m-k-i

      extend :: Int -> [String] -> [String]
      extend i strs' = strs' ++ repeat (replicate i ' ')

      sidebyside :: (Int,Int,Int,[String]) -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
        
      sidebyside (l1,m1,r1,d1) (l2,m2,r2,d2)
        | length d1 > length d2
        = ( l1, m1+r1+2+l2+m2, r2, [ x ++ "  " ++ y | (x,y) <- zip d1 (extend (l2+m2+r2) d2)])
        
        | otherwise
        = ( l1 , m1+r1+2+l2+m2 , r2 , [ x ++ " " ++ y | (x,y) <- zip (extend (l1+m1+r1) d1) d2])


