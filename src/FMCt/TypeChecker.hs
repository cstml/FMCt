module FMCt.TypeChecker
  ( typeCheck
  , TypeError(..)
  )
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

-- | TypeChecking Error.
data TypeError = SimpleErr String          -- ^ A Simple Error
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


-- | returns if a type is the first to occur at a location
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
  
type Var = String
type TypeVar = Either String T
type Context = [(Var, TypeVar)]
type Judgement = (Context, Tm, TypeVar)

data Derivation
  = Star        Judgement
  | Variable    Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation
  | Fusion      Judgement Derivation Derivation
--  deriving Show 

type DisplayLine
  = ( ( Int     -- ^ Left Offset
      , Int     -- ^ Middle Spacing
      , Int     -- ^ Right Offset
      )  
    , [String]  -- ^ Line 
    )
  
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
      showT :: TypeVar -> String
      showT = \case
        Left x -> x
        Right y -> show y
        
      showC :: Context -> String
      showC = let sCtx (x,t) =  x ++ ": " ++ showT t ++ " " in \case
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

freshTypeVars :: [TypeVar]
freshTypeVars = Left <$> [ mconcat $ [[x],[z],show y]
             | y <- [1..]
             , x <- ['A'..'Z']
             , z <- ['A'..'Z']
             ]
splitStream :: [a] -> ([a],[a])
splitStream x = (l,r) where
  l = snd <$> (filter ( odd . fst ) $ zip [1..] x)
  r = snd <$> (filter ( not . odd . fst ) $ zip [1..] x)              

-- | First step towards a derivation is to create the AST
derive0 :: Term -> Derivation
derive0 = derive0' freshTypeVars
  where
    derive0' :: [TypeVar] -> Term -> Derivation
    derive0' stream term =
      let
        emptyCtx :: Context
        emptyCtx = []

        left = fst . splitStream . tail
        right = snd . splitStream . tail
      in
        case term of
          St
            -> Star (emptyCtx, term, head stream)
          V x St
            -> Variable (emptyCtx, term, head stream)
          V x t
            -> Fusion
                 (emptyCtx, term, head stream)
                 (derive0' (left stream) (V x St))
                 (derive0' (right stream) t)
          B x t lo St
            -> Abstraction
                 (emptyCtx, term, head stream)
                 (derive0' (tail stream) (V x St))
          B x t lo t'
            -> Fusion
                 (emptyCtx, term, head stream)
                 (derive0' (left stream) (B x t lo St))
                 (derive0' (right stream) t')
          P t lo St
            -> Application
                 (emptyCtx, term, head stream)
                 (derive0' (tail stream) t)
          P t lo t'
            -> Fusion
                 (emptyCtx, term, head stream)
                 (derive0' (left stream) (P t lo St))
                 (derive0' (right stream) t')               

ex0 = derive0 (parseFMC "*")
ex1 = derive0 (parseFMC "x.*")
ex2 = derive0 (parseFMC "<x:a>.<x:a>.*")
ex3 = derive0 (parseFMC "[x.*].*")
ex4 = derive0 (parseFMC "x.[x.<x:a>.*].*")

-- | Second step is to add our known variables to the context
derive1 :: Context -> Term -> Derivation
derive1 ctx term = 
  let
    simpleT :: TypeVar
    simpleT = Right $ TConst [T Ho "a"] :=> TConst [T Ho "a"]
  in case term of  
      St -> Star (ctx, St, simpleT)
      V x St -> Variable (ctx, term, simpleT)
      V x t  -> Fusion (ctx, term, simpleT) (derive1 ctx (V x St)) (derive1 ctx t)
      B x t lo St -> Abstraction (ctx, term, simpleT) (derive1 nctx (V x St))
        where
          nctx =  (x, Right t) : ctx
      B x t lo t' -> Fusion (ctx, term, simpleT) (derive1 ctx (B x t lo St)) (derive1 ctx t')
        where
          nctx =  (x, Right t): ctx      
      P t lo St -> Application (ctx, term, simpleT) (derive0 t)
      P t lo t' -> Fusion (ctx, term, simpleT) (derive1 ctx (P t lo St)) (derive1 ctx t')

ex01 = derive1 [] (parseFMC "*")
ex11 = derive1 [] (parseFMC "x.*")
ex21 = derive1 [] (parseFMC "<x:a>.<x:a>.*")
ex31 = derive1 [] (parseFMC "[x.*].*")
ex41 = derive1 [] (parseFMC "x.[x.<x:a>.*].*")
ex51 = derive1 [] (parseFMC "<x:a>.x.*")
