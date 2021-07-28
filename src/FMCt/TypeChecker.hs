{-#LANGUAGE TypeSynonymInstances#-}
{-#LANGUAGE FlexibleInstances#-}
module FMCt.TypeChecker
  ( TError(..)
  , typeCheck
  , derive
  , fuse
  , consumes
  , consume
  )
where

import FMCt.Syntax
import Data.List (sort)
import qualified Data.Map as M
import FMCt.Parsing (parseType, parseFMC)
import Text.Read (readMaybe)
import Data.Monoid
import Control.Monad.State
import Control.Exception

-- | TypeChecking Errors.
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
  = Star        Judgement
  | Variable    Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation
  | Fusion      Judgement Derivation Derivation

type Term = Tm
type TypedTerm = (Term, T)
type TVariable = String

instance Semigroup T where
  TVec []       <> x              = x
  x             <> TVec[]         = x
  TCon ""       <> x              = x
  x             <> TCon ""        = x
  xx@(TCon x)   <> yy@(TCon y)    = TVec [xx,yy]
  TVec x        <> TVec y         = TVec $ x ++ y
  xx@(TLoc l x) <> yy@(TLoc l' y) | l == l' = TLoc l $ x <> y
                                  | otherwise = TVec [xx,yy]
  xx <> yy                        = TVec [xx,yy]

instance Monoid T where
  mempty = TCon ""

--------------------------------------------------------------------------------
-- TypeCheck Function

typeCheck :: Tm -> Derivation
typeCheck = derive

--------------------------------------------------------------------------------
-- Aux Functions
getJudgement = \case
  Star j -> j
  Variable j -> j
  Application j _ -> j
  Abstraction j _ -> j
  Fusion j _ _ -> j

freshTypeVar :: [T]
freshTypeVar = TCon <$> [ mconcat $ [[x],[z],show y]
                        | y <- [1..]
                        , x <- ['A'..'Z']
                        , z <- ['A'..'Z']
                        ]

freshVarTypes :: [T]
freshVarTypes =  TVec . (:[]) <$> freshTypeVar

splitStream :: [a] -> ([a],[a])
splitStream x = (,) l r
  where
    l = snd <$> (filter ( odd . fst ) $ zip [1..] x)
    r = snd <$> (filter ( not . odd . fst ) $ zip [1..] x)              

--------------------------------------------------------------------------------
--

fuseTypesD :: Derivation -> Derivation -> T
fuseTypesD dL dR = ty 
  where
    tL = (getJType . getJudgement) dL :: T 
    tR = (getJType . getJudgement) dR :: T 
    ty = fuse tL tR 

newtype MT = MT T

toMT :: T -> MT
toMT x@(t1 :=> t2) = MT x
toMT _             = error "Not Machine Type"
                                                                           
getJType (_,_,x) = x

-- | Second step is to add our known variables to the context
derive :: Term -> Derivation
derive = derive' freshVarTypes [(St, TCon [] :=> TCon [])]
  where

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

    -- | Merge contexts
    mergeCtx :: Context -> Context -> Context
    mergeCtx ox oy = makeSet ox oy
      where
        makeSet [] x  = x
        makeSet (t@(term,ty):xs)  [] = t : makeSet xs oy 
        makeSet t@((term,ty):xs) ((term',ty'):ys)
          = if term /= term' then makeSet t ys
            else if ty == ty' then makeSet xs oy
                 else throw $ ErrOverride $ "Type Conflict between: "
                      ++ show term ++ ":" ++ show ty ++ " and "
                      ++ show term' ++ ":" ++ show ty'

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

          V x St -> Variable (ctx, term, ty)
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
  x@(TCon "") -> normaliseT

  x@(TVec []) -> normaliseT
  
  x@(TCon _)  -> \case
    y@(TCon "")      ->  x
    y@(TCon _)       -> TVec [x,y]
    y@(TVec [])      -> TVec [x]
    yy@(TVec (y:ys)) -> fuse (fuse x y) (TVec ys)
    y@(TLoc l t)     -> TVec [x,y]
    y@(t1 :=> t2)    -> either (throw . ErrWrongT) normaliseT $ consume y (mempty :=> x)
    
  xxx@(TVec xx@(x:xs))  -> \case
    y@(TCon "")      -> xxx
    y@(TCon _)       -> TVec $ xx ++ [y]
    y@(TVec [])      -> xxx
    yy@(TVec (y:ys)) -> fuse (fuse xxx y) (TVec ys)
    y@(TLoc l t)     -> TVec [x,y]
    y@(t1 :=> t2)    -> either (throw . ErrWrongT) normaliseT $ consume y (mempty :=> xxx)
    
  x@(TLoc l t) -> \case
    y@(TCon "")      -> x
    y@(TCon _)       -> TVec $ x : y : []
    y@(TVec [])      -> x
    yy@(TVec (y:ys)) -> fuse (fuse x y) (TVec ys)
    y@(TLoc l' t')   -> if l == l'
                        then TLoc l $ fuse t t'
                        else TVec [x,y]
    y@(t1 :=> t2)    -> either (throw . ErrWrongT) normaliseT $ consume y (mempty :=> x)

  x@(t1 :=> t2)  -> \case
    y@(TCon "")      -> x
    y@(TCon _)       -> fuse (mempty :=> y) x
    y@(TVec [])      -> x
    yy@(TVec (y:ys)) -> fuse (fuse x y) (TVec ys)
    y@(TLoc l' t')   -> fuse (mempty :=> y) x
    y@(t1' :=> t2')  -> either (throw . ErrWrongT) normaliseT $ consume x y

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

  xxx@(TVec xx@(x:xs)) -> \case
    TCon "" -> (,) xxx mempty
    TVec [] -> (,) xxx mempty
    yyy@(TVec yy@(y:ys)) -> let
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
                        
          res' = if res == mempty || res == TVec []
                 then mempty
                 else TLoc l res
                     
          left' = if left == mempty || left == TVec []
                  then mempty
                  else TLoc l left
        in
          (,) res left'

        -- Otherwise they don't interact 
      else (,) x y

    -- TLoc doesn't interact with any other type.
    y       -> (,) x y

  x@(tIn :=> tOut) ->
    \y ->
      if x == y
      then (,) mempty mempty
      else (,) x y 

-- | Normalise gets rid of empty Types at locations.
normaliseT :: T -> T
normaliseT x@(TCon _)         = x
normaliseT x@(TVec [])        = mempty
normaliseT (TLoc _ (TVec [])) = mempty
normaliseT (TLoc _ (TCon "")) = mempty
normaliseT (TVec (x:[]))      = normaliseT x
normaliseT (t1 :=> t2)        = normaliseT t1 :=> normaliseT t2
normaliseT (TVec x)           = TVec $ fEmpty $ normaliseT <$> x
  where
    fEmpty :: [T] -> [T]
    fEmpty = filter (not . aux)
    aux x = x == (mempty :: T) || x == TCon "" || x == TVec []
normaliseT x                  = id x -- Just to be sure it gets through.

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

-- Show Instance
type DisplayLine = (( Int, Int, Int), [String])

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
      showD (Abstraction j d) = addrule (showJ j) (showD d)
      showD (Application j d) = addrule (showJ j) (showD d)
      showD (Fusion j d e) = addrule (showJ j) (sidebyside (showD d) (showD e))

      addrule :: String -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
      addrule x (l,m,r,xs)
        | k <= m     = (ll,k,rr, (replicate ll ' ' ++ x ++ replicate rr ' ')
                                 : showL  l m r
                                 : xs)

        | k <= l+m+r
        = ( ll
          , k
          , rr
          , (replicate ll ' ' ++ x ++ replicate rr ' ')
            : showL ll k rr
            : xs
          )

        | otherwise
        = ( 0
          , k
          , 0
          , x
            : replicate k '-'
            : [ replicate (-ll) ' '
                ++ y
                ++ replicate (-rr) ' '
              | y <- xs
              ]
          )
        where
          k = length x
          i = div (m - k) 2
          ll = l+i
          rr = r+m-k-i

      extend :: Int -> [String] -> [String]
      extend i strs = strs ++ repeat (replicate i ' ')

      sidebyside
        :: (Int,Int,Int,[String])
        -> (Int,Int,Int,[String])
        -> (Int,Int,Int,[String])
        
      sidebyside (l1,m1,r1,d1) (l2,m2,r2,d2)
        | length d1 > length d2
        = ( l1
          , m1+r1+2+l2+m2
          , r2
          , [ x ++ "  " ++ y
            | (x,y) <- zip d1 (extend (l2+m2+r2) d2)
            ]
          )
        | otherwise
        = ( l1
          , m1+r1+2+l2+m2
          , r2
          , [ x ++ "  " ++ y
            | (x,y) <- zip (extend (l1+m1+r1) d1) d2
            ]
          )
