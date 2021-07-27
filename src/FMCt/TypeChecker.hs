{-#LANGUAGE TypeSynonymInstances#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE FlexibleContexts#-}
module FMCt.TypeChecker
  ( typeCheck
  , TypeError(..)
  )
where

import FMCt.Syntax
import Data.List (sort)
import qualified Data.Map as M
import FMCt.Parsing (parseType, parseFMC)
import Text.Read (readMaybe)
import Data.Monoid
import Control.Monad.State

-- | TypeChecking Error.
data TypeError
  = SimpleErr String          -- ^ A Simple Error
  | ClashErr String           -- ^ A Type Clash
  | InadequateTypeErr Tm      -- ^ Type not High
  deriving Show

newtype TypeInference = TI (Either (Maybe T) TypeError)

instance Semigroup TypeInference  where
  TI t1 <> TI t2 = TI $ t1 <> t2

instance Monoid TypeInference where
  mempty = TI $ Left Nothing 

newtype TypeCheck a = TypeCheck {getTest :: Tm -> a}

instance Functor TypeCheck where
  fmap f (TypeCheck m) = TypeCheck $ \term -> f (m term)

instance Applicative TypeCheck where
  pure t = TypeCheck $ const t
  TypeCheck f <*> TypeCheck i = TypeCheck $
    \term -> let x = i term; y = f term in  y x

instance Semigroup a => Semigroup (TypeCheck a) where
  TypeCheck t1 <> TypeCheck t2 = TypeCheck $ \term -> t1 term <> t2 term

instance Monoid a => Monoid (TypeCheck a) where
  mempty = TypeCheck $ const mempty

typeCheck :: Tm -> Maybe T
typeCheck t =
  case tests t of
    TI (Left t) -> t
    TI (Right e) -> error $ show e
  where tests = getTest $ mconcat [ higherType ]

-- | Tests if all the terms are higher typed 
higherType :: TypeCheck TypeInference
higherType = fmap TI $ TypeCheck $ \term -> f term
  where
    err = InadequateTypeErr 
    f term =
          case term of
            V _ t --  If it is a Variable it doesn't matter
              -> Left Nothing <> f t
            P t _ t' --  If it is an Application it doesn't matter
              -> Left Nothing <> f t <> f t'
            B _ ty _ t --  If it is an Abstraction we check the term is of higher kind
              -> f t <> case ty of
                       _ :=> _ -> Left Nothing
                       _ -> Right $ err term
            St -- If it is Star we reached the end of the term
              -> Left Nothing


-- | Returns if a type occurs inside a type.
occurs :: T -> T -> Bool
occurs t t'
  | t == t'   = True
  | otherwise =  case t' of
      TCon _ -> False
  
type Context = [(Term, T)]
type Judgement = (Context, Term, T)

data Derivation
  = Star        Judgement
  | Variable    Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation
  | Fusion      Judgement Derivation Derivation

type DisplayLine = (( Int, Int, Int), [String])
  
nextJudgement :: Derivation -> [Judgement]
nextJudgement = \case
  Star _ -> []
  Variable _ -> []
  Application _ d -> getJudgement <$> [d]
  Abstraction _ d -> getJudgement <$> [d]                    
  Fusion _ d1 d2  -> getJudgement <$> [d1, d2]

getJudgement = \case
  Star j -> j
  Variable j -> j
  Application j _ -> j
  Abstraction j _ -> j
  Fusion j _ _ -> j

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

type Term = Tm
type TypedTerm = (Term, T)

type TVariable = String

freshTypeVar :: [T]
freshTypeVar = TCon <$> [ mconcat $ [[x],[z],show y]
            | y <- [1..]
            , x <- ['A'..'Z']
            , z <- ['A'..'Z']
            ]

freshVarTypes :: [T]
freshVarTypes =  (\x -> TVec [x])  <$> freshTypeVar

splitStream :: [a] -> ([a],[a])
splitStream x = (l,r) where
  l = snd <$> (filter ( odd . fst ) $ zip [1..] x)
  r = snd <$> (filter ( not . odd . fst ) $ zip [1..] x)              


-- | First step towards a derivation is to create the AST
derive0 :: Term -> Derivation
derive0 term = 
  let
    emptyCtx :: Context
    emptyCtx = []

    left = fst . splitStream . tail
    right = snd . splitStream . tail

    emptyType = TCon "" :=> TCon ""
      in
        case term of
          St
            -> Star (emptyCtx, term, emptyType)
          V x St
            -> Variable (emptyCtx, term, emptyType)
          V x t
            -> Fusion
                 (emptyCtx, term, emptyType)
                 (derive0 (V x St))
                 (derive0 t)
          B x t lo St
            -> Abstraction
                 (emptyCtx, term, emptyType)
                 (derive0 (V x St))
          B x t lo t'
            -> Fusion
                 (emptyCtx, term, emptyType)
                 (derive0 (B x t lo St))
                 (derive0 t')
          P t lo St
            -> Application
                 (emptyCtx, term, emptyType)
                 (derive0 t)
          P t lo t'
            -> Fusion
                 (emptyCtx, term, emptyType)
                 (derive0 (P t lo St))
                 (derive0  t')               

ex0 = derive0 (parseFMC "*")
ex1 = derive0 (parseFMC "x.*")
ex2 = derive0 (parseFMC "<x:a>.<x:a>.*")
ex3 = derive0 (parseFMC "[x.*].*")
ex4 = derive0 (parseFMC "x.[x.<x:a>.*].*")
  

-- | Second step is to add our known variables to the context
derive1 :: Term -> Derivation
derive1 = derive1' freshVarTypes
  where
    eT :: T
    eT = TVec []

    gCtx :: Derivation -> Context
    gCtx = \case
      Star (c,_,_) -> c
      Variable (c,_,_) -> c
      Abstraction (c,_,_) _ -> c
      Application (c,_,_) _ -> c
      Fusion (c,_,_) _ _ -> c

    derive1' :: [T] -> Term -> Derivation
    derive1' stream term
      = case term of  
          St -> Star ([] ,St ,eT)
          V x St -> Variable (ctx, term, ty)
            where
              ctx = [(term, ty)]
              ty = head stream
          V x t  -> Fusion (nctx, term, ty) leftD rightD
            where
              ty = head stream

              leftD = (derive1' nStreamL (V x St))
              rightD = (derive1' nStreamR t)

              nStream = tail stream
              (nStreamL,nStreamR) = splitStream nStream

              nctx = gCtx leftD ++ gCtx rightD
              
          B x t lo St -> Abstraction (nctx, term, ty) nDeriv
            where              
              ty = head stream
              nStream = tail stream
              nDeriv = derive1' nStream (V x St)
              nctx =  (V x St, t) : (gCtx nDeriv)
              
          B x t lo t' -> Fusion (nctx, term, ty) dLeft dRight                                  
            where
              dLeft = derive1' nStreamL (B x t lo St)
              dRight = derive1' nStreamR  t'
              nctx =  (V x St, t): (gCtx dLeft) ++ (gCtx dRight)
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St -> Application (ctx, term, ty) nDeriv
            where
              ty = head stream
              nDeriv = derive1' nStream t
              nStream = tail stream
              ctx = gCtx nDeriv            

          P t lo t' -> Fusion (nctx, term, ty) dLeft dRight
            where
              dLeft = derive1' nStreamL (P t lo St)
              dRight = derive1' nStreamR t'
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream
              nctx = []              

ex01  = derive1 (parseFMC "*")
ex11  = derive1 (parseFMC "x.y.*")
ex21' = derive1 (parseFMC "<x:a>.*")
ex21  = derive1 (parseFMC "<x:a>.<x:a>.*")
ex31  = derive1 (parseFMC "[x.*].*")
ex41  = derive1 (parseFMC "x.[x.<x:a>.*].*")
ex51  = derive1 (parseFMC "<x:a>.x.*")

{-
-- | Second step is to add our known variables to the context
derive2 :: Term -> Derivation
derive2 = derive2' freshVarTypes []
  where
    eT :: T
    eT = Left <$> TVec []

    gCtx :: Derivation -> Context
    gCtx = \case
      Star (c,_,_) -> c
      Variable (c,_,_) -> c
      Abstraction (c,_,_) _ -> c
      Application (c,_,_) _ -> c
      Fusion (c,_,_) _ _ -> c

    getType :: Term -> Context -> T
    getType t [] = error $ "Cannot Find type for term: " ++ show t ++ " in context. Have you defined it prior to calling it ?"
    getType t ((t',ty):xs) = if t == t' then ty else getType t xs
    
    derive2' :: [T] -> Context -> Term -> Derivation
    derive2' stream ctx term 
      = case term of  
          St -> Star ([] ,St ,eT)

          V x St -> Variable (ctx, term, ty)
            where
              ty =  getType term ctx

          V x t  -> Fusion (ctx, term, ty) leftD rightD
            where
              ty = head stream
              leftD = (derive2' nStreamL ctx (V x St))
              rightD = (derive2' nStreamR ctx t)
              nStream = tail stream
              (nStreamL,nStreamR) = splitStream nStream
              
          B x t lo St -> Abstraction (ctx, term, ty) nDeriv
            where              
              ty = head stream
              nStream = tail stream
              nDeriv = derive2' nStream nctx (V x St)
              nctx = (V x St, Right <$> t) : ctx 
              nctx' =  gCtx nDeriv
              
          B x t lo t' -> Fusion (ctx, term, ty) dLeft dRight                                   
            where
              dLeft = derive2' nStreamL ctx (B x t lo St)
              dRight = derive2' nStreamR  nctx t'
              nctx = (V x St, Right <$> t) : ctx 
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St -> Application (ctx, term, ty) nDeriv
            where
              ty = head stream
              nDeriv = derive2' nStream ctx t
              nStream = tail stream

          P t lo t' -> Fusion (nctx, term, ty) dLeft dRight
            where
              dLeft = derive2' nStreamL ctx (P t lo St)
              dRight = derive2' nStreamR ctx t'
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream
              nctx = []              

ex02 = derive2 (parseFMC "*")
ex12 = derive2 (parseFMC "x.y.*")
ex22 = derive2 (parseFMC "<x:a>.*")
ex32 = derive2 (parseFMC "<x:a>.x.*")
ex32' = derive2 (parseFMC "<x:a>.<y:a>.x.*")
ex42 = derive2 (parseFMC "[x.*].*")
ex52 = derive2 (parseFMC "[<x:a>.x.*].*")
ex62 = derive2 (parseFMC "<x:a>.x.*")
-}

-- | Second step is to add our known variables to the context
derive3 :: Term -> Derivation
derive3 = derive3' freshVarTypes [(St, TVec [] :=> TVec[])]
  where
    eT :: T
    eT = TVec []

    gCtx :: Derivation -> Context
    gCtx = \case
      Star (c,_,_) -> c
      Variable (c,_,_) -> c
      Abstraction (c,_,_) _ -> c
      Application (c,_,_) _ -> c
      Fusion (c,_,_) _ _ -> c
    
    getType :: Term -> Context -> T
    getType t [] = error $ "Cannot Find type for term: " ++ show t
                         ++ " in context. Have you defined it prior to calling it ?"
    getType t ((t',ty):xs) = if t == t' then ty else getType t xs

    mergeCtx :: Context -> Context -> Context
    mergeCtx ox oy = makeSet ox oy
      where
        makeSet [] x  = x
        makeSet (t@(term,ty):xs)  [] = t : makeSet xs oy 
        makeSet t@((term,ty):xs) ((term',ty'):ys)
          = if term /= term' then makeSet t ys
            else if ty == ty' then makeSet xs oy
                 else error $ "Type Conflict between: "
                      ++ show term ++ ":" ++ show ty ++ " and "
                      ++ show term' ++ ":" ++ show ty'
    
    derive3' :: [T] -> Context -> Term -> Derivation
    derive3' stream ctx term 
      = case term of  
          St -> Star (ctx ,St ,eT)

          V x St -> Variable (ctx, term, ty)
            where
              ty =  getType term ctx

          V x t  -> Fusion (ctx, term, ty) leftD rightD
            where
              ty = head stream
              leftD = (derive3' nStreamL ctx (V x St))
              rightD = (derive3' nStreamR ctx t)
              nStream = tail stream
              (nStreamL,nStreamR) = splitStream nStream
              
          B x t lo St -> Abstraction (ctx', term, ty) nDeriv
            where              
              ty = TLoc lo t :=> TVec []
              nStream = tail stream
              nDeriv = derive3' nStream nctx (V x St)
              nctx = (V x St, t) : ctx 
              ctx' = (term,ty) : gCtx nDeriv  
              
          B x t lo t' -> Fusion (ctx', term, ty) dLeft dRight                                   
            where
              dLeft = derive3' nStreamL ctx (B x t lo St)
              dRight = derive3' nStreamR  nctx t'
              nctx = (V x St, t) : ctx
              ctx' = (term,ty) : mergeCtx (gCtx dLeft) (gCtx dRight)
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St -> Application (ctx', term, ty) nDeriv
            where
              ty = TVec [] :=> getType t ctx'
              nDeriv = derive3' nStream ctx t
              nStream = tail stream
              ctx' = (term,ty) : gCtx nDeriv

          P t lo t' -> Fusion (nctx, term, ty) dLeft dRight
            where
              dLeft = derive3' nStreamL ctx (P t lo St)
              dRight = derive3' nStreamR ctx t'
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream
              nctx = (term,ty): mergeCtx (gCtx dLeft) (gCtx dRight)              

ex03 = derive3 (parseFMC "*")
ex13 = derive3 (parseFMC "x.y.*")
ex23 = derive3 (parseFMC "<x:a>.*")
ex33 = derive3 (parseFMC "<x:a>.x.*")
ex33' = derive3 (parseFMC "<x:a>.<y:a>.x.*")
ex43 = derive3 (parseFMC "[x.*].*")
ex53 = derive3 (parseFMC "[<x:a>.x.*].*")
ex63 = derive3 (parseFMC "[<x:(=>a)>.x.*].<y:a>.*")
ex73 = derive3 (parseFMC "[*].*")
ex83 = derive3 (parseFMC "[*].<x:(=>)>.*")

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

fuseTypesD :: Derivation -> Derivation -> Maybe T
fuseTypesD dL dR = ty 
  where
    tL = (getJType . getJudgement) dL :: T
    tR = (getJType . getJudgement) dR :: T
    ty = Just $ fuse tL tR

fuse :: T -> T -> T
fuse = \case
  TCon ""      -> id
  xx@(TCon x)  -> \case
    yy@(TCon y)    -> TVec $ xx : yy : []
    yy@(TVec y)    -> TVec $ xx : y
    yy@(TLoc l y)  -> TVec $ xx : yy : []
    yy@(t1 :=> t2) -> fuse (TCon "" :=> xx) yy

  xx@(TVec x) -> \case
    yy@(TCon y)    -> TVec $ x ++ [yy]
    yy@(TVec y)    -> TVec $ x ++ y
    yy@(TLoc l y)  -> TVec $ x ++ [yy]
    yy@(t1 :=> t2) -> fuse (TCon "" :=> xx) yy

  xx@(TLoc lx x) -> \case
    yy@(TCon y)    -> TVec $ xx : yy : []
    yy@(TVec y)    -> TVec $ [xx] ++ y
    yy@(TLoc l y)  -> TVec $ [xx] ++ [yy]
    yy@(t1 :=> t2) -> fuse (TCon "" :=> xx) yy

  xx@(xTi :=> xTo) -> \case
    yy@(yTi :=> yTo) -> if consumes yTi xTo 
                         then  fuse xTi (consume  yTi xTo) :=> yTo                       
                         else  error "Cannot fuse"
                    
consumes :: T -> T -> Bool
consumes xx@(TCon x)    yy@(TCon y)   = xx == yy
consumes xx@(t1 :=> t2) yy@(TCon y)   = consumes t1 yy 
consumes xx             yy@(TVec [])  = True
consumes (TVec [])      _             = False

consume :: T -> T -> T
consume = (<->)

newtype MT = MT T

pack :: T -> MT
pack x@(t1 :=> t2) = MT x
pack _             = error "Not Machine Type"



-- | Fully Consumes one Another
(<<->>) :: T -> T -> Maybe T
TCon x <<->> TCon y
  | x == y    = Just mempty
  | otherwise = Nothing
TVec x <<->> TVec y
  | x == y    = Just mempty
  | otherwise = Nothing
TLoc l t <<->> TLoc l' t'
  | l == l' && t == t' = Just mempty
  | otherwise = Nothing
(t1 :=> t2) <<->> (t1' :=> t2')
  | t2 == t1' = Just $ t1 :=> t2'
  | otherwise = Nothing

(<->) :: T -> T -> T
TCon x  <-> TCon y
  | x == y       = mempty
  | otherwise    = TCon x
TVec []  <-> yy  = mempty
TCon ""  <-> yy  = mempty
TLoc l t <-> TLoc l' t'
  | l == l'      = if t == t'
                  then mempty
                  else error "Different Location Types"
  |otherwise     =  TLoc l t
TVec(x:xs) <-> y =  x <-> y <> ((TVec xs) <-> (x <-> y))
(t1 :=> t2)  <-> y = if t1 <-> y == mempty
                     then mempty :=> t2
                     else error $ "did not consume everything"
x <-> TVec (y:ys) = x <-> y <> ((x<->y) <-> TVec ys)
x <-> TVec []     = x
x <-> TCon ""     = x

                               

cEx1 = TCon "a" <-> TCon "a"
cEx2 = TCon "a" <-> TVec [TCon "a", TCon "a"]

cEx3 = TVec [TCon "a", TCon "a"] <-> TVec [TCon "a", TCon "a", TCon "b"]
cEx4 = TVec [TCon "a", TCon "a", TCon "b"] <-> TVec [TCon "a", TCon "a"]

cEx5 = TVec[TCon "b"] <-> TVec [TCon "a", TCon "a"]

cEx6 = TVec[TCon "a", TCon "a"] <<->> TVec [TCon "a", TCon "a"]

typeToConsume :: T -> State T ()
typeToConsume t = void $ put t

consumeT :: T -> State T ()
consumeT t = do
  state <- get
  put (state <-> t)
                                                                           
getJType (_,_,x) = x

-- | Second step is to add our known variables to the context
derive4 :: Term -> Derivation
derive4 = derive4' freshVarTypes [(St, TCon [] :=> TCon [])]
  where
    eT :: T
    eT = TCon []

    -- | Get the context 
    gCtx :: Derivation -> Context
    gCtx = \case
      Star        (c,_,_)     -> c
      Variable    (c,_,_)     -> c
      Abstraction (c,_,_) _   -> c
      Application (c,_,_) _   -> c
      Fusion      (c,_,_) _ _ -> c
    
    getType :: Term -> Context -> T
    getType t [] = error $ "Cannot Find type for term: " ++ show t
                         ++ " in context. Have you defined it prior to calling it ?"
    getType t ((t',ty):xs) = if t == t' then ty else getType t xs

    mergeCtx :: Context -> Context -> Context
    mergeCtx ox oy = makeSet ox oy
      where
        makeSet [] x  = x
        makeSet (t@(term,ty):xs)  [] = t : makeSet xs oy 
        makeSet t@((term,ty):xs) ((term',ty'):ys)
          = if term /= term' then makeSet t ys
            else if ty == ty' then makeSet xs oy
                 else error $ "Type Conflict between: "
                      ++ show term ++ ":" ++ show ty ++ " and "
                      ++ show term' ++ ":" ++ show ty'
    
    getUpperType :: Derivation -> T
    getUpperType = \case 
      Star        (_,_,t)     -> t 
      Variable    (_,_,t)     -> t 
      Abstraction (_,_,t) _   -> t
      Application (_,_,t) _   -> t
      Fusion      (_,_,t) _ _ -> t        
    
    derive4' :: [T] -> Context -> Term -> Derivation
    derive4' stream ctx term 
      = case term of  
          St -> Star (ctx, St, ty)
            where
              ty = getType term ctx

          V x St -> Variable (ctx, term, ty)
            where
              ty = getType term ctx

          V x t  -> Fusion (ctx, term, ty) leftD rightD
            where
              ty                  = head stream
              leftD               = derive4' nStreamL ctx (V x St)
              rightD              = derive4' nStreamR ctx t
              nStream             = tail stream
              (nStreamL,nStreamR) = splitStream nStream
              
          B x t lo St -> Abstraction (ctx', term, ty) nDeriv
            where              
              ty      = TLoc lo t :=> TCon []
              nStream = tail stream
              nDeriv  = derive4' nStream ctx' (V x St)
              ctx'    = (V x St, t) : ctx 
              
          B x t lo t' -> Fusion (ctx', term, ty) dLeft dRight
            where
              dLeft    = derive4' nStreamL ctx (B x t lo St)
              dRight   = derive4' nStreamR ctx' t'
              ctx'     = (V x St, t) : ctx
              ty       = maybe (error "Bad Fusion") id $ fuseTypesD dLeft dRight
              nStream  = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St -> Application (ctx, term, ty) nDeriv
            where
              uty = getUpperType 
              ty = TCon [] :=> TCon []
              nDeriv = derive4' nStream ctx t
              nStream = tail stream

          P t lo t' -> Fusion (nctx, term, ty) dLeft dRight
            where
              dLeft = derive4' nStreamL ctx (P t lo St)
              dRight = derive4' nStreamR ctx t'
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream
              nctx =  mergeCtx (gCtx dLeft) (gCtx dRight)              

      
ex04 = derive4 $ parseFMC "*"
ex14 = derive4 $ parseFMC "x.y.*"
ex24 = derive4 $ parseFMC "<x:(=>a)>.*"
ex34 = derive4 $ parseFMC "<x:a>.x.*"
ex44 = derive4 $ parseFMC "<x:a>.<y:a>.x.*"
ex54 = derive4 $ parseFMC "[x.*].*"
ex64 = derive4 $ parseFMC "[<x:a>.x.*].*"
ex74 = derive4 $ parseFMC "[<x:(=>a)>.x.*].<y:a>.*"
ex84 = derive4 $ parseFMC "[*].*"
ex94 = derive4 $ parseFMC "[*].<x:(=>)>.*"


{-
x.y.z
             y.*     z.* 
~> x.*        y.z.*


    x.*
-----------
   <x:a>.*


    x.*
-----------
   [x.*].*        y.*
-------------------------
  [x.*].y.*
                     *        
                   -------    -------
         *          [*].*       y.*
       --------    -------------------
        [*].*       [*].y.*
        ----------------
x.*     [*].[*].y.*
-----------------------
x.[*].[*].y.*

-}

fuseN :: T -> T -> T
fuseN = \case
  x@(TCon "") -> id
  x@(TVec []) -> id
  
  x@(TCon _)  -> \case
    y@(TCon "")      ->  x
    y@(TCon _)       -> TVec [x,y]
    y@(TVec [])      -> TVec [x]
    yy@(TVec (y:ys)) -> fuseN (fuseN x y) (TVec ys)
    y@(TLoc l t)     -> TVec [x,y]
    y@(t1 :=> t2)    -> either error id $ consumeN y (mempty :=> x)
    
  xxx@(TVec xx@(x:xs))  -> \case
    y@(TCon "")      -> xxx
    y@(TCon _)       -> TVec $ xx ++ [y]
    y@(TVec [])      -> xxx
    yy@(TVec (y:ys)) -> fuseN (fuseN xxx y) (TVec ys)
    y@(TLoc l t)     -> TVec [x,y]
    y@(t1 :=> t2)    -> either error id $ consumeN y (mempty :=> xxx)
    
  x@(TLoc l t)  -> \case
    y@(TCon "")      -> x
    y@(TCon _)       -> TVec $ x : y : []
    y@(TVec [])      -> x
    yy@(TVec (y:ys)) -> fuseN (fuseN x y) (TVec ys)
    y@(TLoc l' t')   -> if l == l'
                        then TLoc l $ fuseN t t'
                        else TVec [x,y]
    y@(t1 :=> t2)    -> either error id $ consumeN y (mempty :=> x)

  x@(t1 :=> t2)  -> \case
    y@(TCon "")      -> x
    y@(TCon _)       -> fuseN (mempty :=> y) x
    y@(TVec [])      -> x
    yy@(TVec (y:ys)) -> fuseN (fuseN x y) (TVec ys)
    y@(TLoc l' t')   -> fuseN (mempty :=> y) x
    y@(t1' :=> t2')    -> either error id $ consumeN y x

consumesN :: T
          -- ^ Requires - What the term will consume
          -> T
          -- ^ Receives - what the term is getting 
          -> (T,T)
          -- ^ (Left from Requires, Left from Receives)
consumesN = \case
  TCon "" -> (,) mempty . id

  TVec [] -> (,) mempty . id

  xx@(TCon x)  -> \case
    TCon "" -> (,) xx mempty
    yy@(TCon y) -> if x == y then (,) mempty mempty
                   else error $
                        mconcat [ "cannot merge ", show xx , " " , show yy ]
    TVec [] -> (,) xx mempty
    TVec (y:ys) -> let
        (res1, left1) = consumesN xx y
        (res2, left2) = consumesN res1 (TVec ys)
      in
        (,) res2 (left1 <> left2)        
    y -> (xx,y)

  xxx@(TVec xx@(x:xs)) -> \case
    TCon "" -> (,) xxx mempty
    TVec [] -> (,) xxx mempty
    yyy@(TVec yy@(y:ys)) -> let
        (res1,left1) = consumesN xxx y
        (res2,left2) = consumesN res1 (TVec ys)
      in
        (,) res2 (left1 <> left2)
    y -> let
        (res1,left1) = consumesN x y
        (res2,left2) = consumesN (TVec xs) left1
      in
        (,) (res1 <> res2) (left2)

  x@(TLoc l t) -> \case
    TCon "" -> (,) x mempty
    TVec [] -> (,) x mempty
    y@(TLoc l' t') ->
      -- ^ If the location are the same we need to check what happens with the
      -- interaction between the types.
      if l == l'
      then
        let              
          (res, left) = consumesN t t'
                        
          res' = if res == mempty || res == TVec []
                 then mempty
                 else TLoc l res
                     
          left' = if left == mempty || left == TVec []
                  then mempty
                  else TLoc l left
        in
          (,) res left'

        -- | Otherwise they don't interact 
      else (,) x y

    -- | TLoc doesn't interact with any other type.
    y       -> (,) x y

  x@(tIn :=> tOut) ->
    \y ->
      if x == y
      then (,) mempty mempty
      else (,) x y 
      
xc1 = consumesN (TCon "x") (TCon "x")
xc2 = consumesN (TVec [ TCon "y", TCon "x", TCon "z" ]) (TCon "y")
-- | One extra Received
xc3 = consumesN (TVec [ TCon "y", TCon "x", TCon "z" ])
      (TVec [ TCon "y", TCon "x", TCon "z", TCon "p" ])
-- | One extra Required
xc4 = consumesN
      (TVec [ TCon "y", TCon "x", TCon "z", TCon "p" ])
      (TVec [ TCon "y", TCon "x", TCon "z" ])
xc5 = consumesN (TVec [ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y" ])

-- | The inputs at the locations do interact.
xc6 = consumesN (TVec [ TLoc Ho $ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y" ])

-- | The inputs at the locations do interact but leave some behind.
xc7 = consumesN (TVec [ TCon "x", TLoc Ho $ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y" ])

-- | This should saturate, even though the inputs are reversed
xc8 = consumesN (TVec [ TCon "x", TLoc Ho $ TCon "y" ]) (TVec [ TLoc Ho $ TCon "y", TCon "x" ])
                            
consumeN :: T -> T -> Either String T
consumeN t1@(tIn :=> tOut) t2@(tIn' :=> tOut') = result  
  where
   intermediary :: (T,T)
   intermediary = tIn' `consumesN` tOut

   result :: Either String T
   result = case intermediary of
     (TCon "", TCon "") -> Right $ tIn :=> tOut'
     (TCon "", TVec []) -> Right $ tIn :=> tOut'
     (TCon "", x)       -> Right $ (tIn <> x) :=> tOut'
     (TVec [], TCon "") -> Right $ tIn :=> tOut'
     (TVec [], TVec []) -> Right $ tIn :=> tOut'
     (TVec [], x)       -> Right $ (tIn <> x) :=> tOut'
     _ -> Left $ mconcat $
            [ "error could not merge "
            , show t1
            , " "
            , show t2
            , " "
            , show intermediary
            ]

exCons :: Either String T
-- | Simple fusion example.
exCons = (TCon "a" :=> TCon "b") `consumeN` (TCon "b" :=> TCon "c")
-- | This Time wrapped in a vector.
exCons' = (TCon "a" :=> TVec[TCon "b"]) `consumeN` (TCon "b" :=> TCon "c")
-- | Wrapped in two vectors.
exCons'' = (TCon "a" :=> TVec[TVec[TCon "b"]]) `consumeN` (TCon "b" :=> TCon "c")

exCons2 = (TCon "a" :=> TCon "b") `consumeN` (TCon "" :=> TCon "c")
