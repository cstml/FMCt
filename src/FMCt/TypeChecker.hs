module FMCt.TypeChecker
--  ( typeCheck
--  , TypeError(..)
--  )
where

import FMCt.Syntax
import Data.List (sort)
import Data.Semigroup
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.ST
import FMCt.Parsing
import Text.Read (readMaybe)
import Control.Applicative
{-
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


-- | Returns if a type is the first to occur at a location
occurs :: T -> T -> Bool
occurs tt@(TConst ((T l t):[])) =
  \case 
    TConst (x:xs)
      -> case x of
           T l' t'
             -> if l == l' then
                  if t == t' then True
                  else False
                else occurs tt (TConst xs)
    _ -> False

sameLo :: T -> T -> Bool
sameLo (TConst ((T l _) :[]))
  = \case
  (TConst ((T l' _):[])) -> l == l'
  (TLocat l' _) -> l == l'
  _ -> False
  
sameLo (TLocat l _)
  = \case
  (TLocat l' _) -> l == l'
  (TConst ((T l' _):[])) -> l == l'
  _ -> False

(>-<) :: T -> T -> Either T TypeError
TConst (x:xs) >-< TConst (x':xs') = undefined
TConst [] >-< y = Left y
  
type Context = [(Term, TType)]
type Judgement = (Context, Term, TType)

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
      showT :: TType -> String
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
type TypedTerm = (Term, TType)

freshTypeVar :: [TVariable]
freshTypeVar = [ mconcat $ [[x],[z],show y]
            | y <- [1..]
            , x <- ['A'..'Z']
            , z <- ['A'..'Z']
            ]

freshVarTypes :: [TType]
freshVarTypes =  (\x -> Left <$> TConst [x])  <$> freshTypeVar

splitStream :: [a] -> ([a],[a])
splitStream x = (l,r) where
  l = snd <$> (filter ( odd . fst ) $ zip [1..] x)
  r = snd <$> (filter ( not . odd . fst ) $ zip [1..] x)              

{-
-- | First step towards a derivation is to create the AST
derive0 :: Term -> Derivation
derive0 term = 
  let
    emptyCtx :: Context
    emptyCtx = []

    left = fst . splitStream . tail
    right = snd . splitStream . tail

    emptyType = Left <$> (TConst $  ["a"])
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
    eT :: TType
    eT = Left <$> TConst []

    gCtx :: Derivation -> Context
    gCtx = \case
      Star (c,_,_) -> c
      Variable (c,_,_) -> c
      Abstraction (c,_,_) _ -> c
      Application (c,_,_) _ -> c
      Fusion (c,_,_) _ _ -> c

    derive1' :: [TType] -> Term -> Derivation
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
              nctx =  (V x St, Right <$> t) : (gCtx nDeriv)
          B x t lo t' -> Fusion
                         (nctx, term, ty)
                         dLeft
                         dRight
                                   
            where
              dLeft = derive1' nStreamL (B x t lo St)
              dRight = derive1' nStreamR  t'
              nctx =  (V x St, Right <$> t): (gCtx dLeft) ++ (gCtx dRight)
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

type Subs = (TType,TType)
{-
-- | Second step is to add our known variables to the context
derive2 :: Term -> Derivation
derive2 = derive2' freshVarTypes []
  where
    eT :: TType
    eT = Left <$> TConst []

    gCtx :: Derivation -> Context
    gCtx = \case
      Star (c,_,_) -> c
      Variable (c,_,_) -> c
      Abstraction (c,_,_) _ -> c
      Application (c,_,_) _ -> c
      Fusion (c,_,_) _ _ -> c

    getType :: Term -> Context -> TType
    getType t [] = error $ "Cannot Find type for term: " ++ show t ++ " in context. Have you defined it prior to calling it ?"
    getType t ((t',ty):xs) = if t == t' then ty else getType t xs
    
    derive2' :: [TType] -> Context -> Term -> Derivation
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
-}
-- | Second step is to add our known variables to the context
derive3 :: Term -> Derivation
derive3 = derive3' freshVarTypes [(St, Right <$> TConst [] :=> TConst[])]
  where
    eT :: TType
    eT = Left <$> TConst []

    gCtx :: Derivation -> Context
    gCtx = \case
      Star (c,_,_) -> c
      Variable (c,_,_) -> c
      Abstraction (c,_,_) _ -> c
      Application (c,_,_) _ -> c
      Fusion (c,_,_) _ _ -> c
    
    getType :: Term -> Context -> TType
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
    
    derive3' :: [TType] -> Context -> Term -> Derivation
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
              ty = Right <$> (TLocat lo t :=> TConst [])
              nStream = tail stream
              nDeriv = derive3' nStream nctx (V x St)
              nctx = (V x St, Right <$> t) : ctx 
              ctx' = (term,ty) : gCtx nDeriv  
              
          B x t lo t' -> Fusion (ctx', term, ty) dLeft dRight                                   
            where
              dLeft = derive3' nStreamL ctx (B x t lo St)
              dRight = derive3' nStreamR  nctx t'
              nctx = (V x St, Right <$> t) : ctx
              ctx' = (term,ty) : mergeCtx (gCtx dLeft) (gCtx dRight)
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St -> Application (ctx', term, ty) nDeriv
            where
              ty = TConst [] :=> getType t ctx'
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


{-
-- | Second step is to add our known variables to the context
derive4 :: Term -> Derivation
derive4 = derive4' freshVarTypes [(St, Right <$> TConst [] :=> TConst[])]
  where
    eT :: TType
    eT = Left <$> TConst []

    gCtx :: Derivation -> Context
    gCtx = \case
      Star (c,_,_) -> c
      Variable (c,_,_) -> c
      Abstraction (c,_,_) _ -> c
      Application (c,_,_) _ -> c
      Fusion (c,_,_) _ _ -> c

    getJType :: Judgement -> TType
    getJType (_,_,t) = t

    getLocation :: Lo -> TType -> TType
    getLocation l
      = \case
      TConst []     -> error "Cannot Find any"
      TConst (x:xs) -> case x of
        Left _ -> getLocation l (TConst xs)
        Right (T l' c) -> if l == l' then TConst [x]
                          else getLocation l $ TConst xs
      TLocat l' t -> if l == l' then t
                     else error "Wrong Type"
      ty@(_ :=> _) -> ty
    
    getType :: Term -> Context -> TType
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
    
    derive4' :: [TType] -> Context -> Term -> Derivation
    derive4' stream ctx term 
      = case term of  
          St -> Star (ctx ,St ,eT)

          V x St -> Variable (ctx, term, ty)
            where
              ty =  getType term ctx

          V x t  -> Fusion (ctx, term, ty) leftD rightD
            where
              ty = head stream
              leftD = (derive4' nStreamL ctx (V x St))
              rightD = (derive4' nStreamR ctx t)
              nStream = tail stream
              (nStreamL,nStreamR) = splitStream nStream
              
          B x typ lo St -> Abstraction (ctx', term, ty) nDeriv
            where              
              --ty = lType --Right <$> (TLocat lo t :=> TConst [])
              ty = if lType == (Right <$> typ) then lType
                   else error "Can't do the fusion it "

              nStream = tail stream
              nDeriv = derive4' nStream nctx (V x St)
              nctx = (V x St, Right <$> typ) : ctx 
              ctx' = (term,ty) : gCtx nDeriv
              
              lType :: TType
              lType = (getLocation lo . getJType . getJudgement) nDeriv
              
          B x typ lo t' -> Fusion (ctx', term, ty) dLeft dRight                                   
            where
              dLeft = derive4' nStreamL ctx (B x typ lo St)
              dRight = derive4' nStreamR  nctx t'

              lType :: TType
              lType = (getLocation lo . getJType . getJudgement) dLeft

              nctx = (V x St, Right <$> typ) : ctx
              ctx' = (term,ty) : mergeCtx (gCtx dLeft) (gCtx dRight)
              ty = if (Right <$> typ) then lType
                   else error "Can't do the fusion it "
                
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream

          P t lo St -> Application (ctx', term, ty) nDeriv
            where
              ty = TLocat lo (TConst [] :=> getType t ctx')
              nDeriv = derive4' nStream ctx t
              nStream = tail stream
              ctx' = (term,ty) : gCtx nDeriv

          P t lo t' -> Fusion (nctx, term, ty) dLeft dRight
            where
              dLeft = derive4' nStreamL ctx (P t lo St)
              dRight = derive4' nStreamR ctx t'
              ty = head stream
              nStream = tail stream
              (nStreamL, nStreamR) = splitStream nStream
              nctx = (term,ty): mergeCtx (gCtx dLeft) (gCtx dRight)              

ex04 = derive4 (parseFMC "*")
ex14 = derive4 (parseFMC "x.y.*")
ex24 = derive4 (parseFMC "<x:a>.*")
ex34 = derive4 (parseFMC "<x:a>.x.*")
ex34' = derive4 (parseFMC "[*].<x:a>.<y:b>.x.y.*")
ex34'' = derive4 (parseFMC "[*].<x:a>.*")
ex44 = derive4 (parseFMC "[x.*].*")
ex54 = derive4 (parseFMC "[<x:a>.x.*].*")
ex64 = derive4 (parseFMC "[<x:(=>a)>.x.*].<y:a>.*")
ex74 = derive4 (parseFMC "[*].<x:(=>)>.*")
ex84 = derive4 (parseFMC "[*].<x:(a=>)>.x.*")
-}
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

-}
