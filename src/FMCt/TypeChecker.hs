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
type Context = [(Var, T)]
type Judgement = (Context, Tm, T)

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

-- -......_..._
-- 0 .....0...0
-- - .....-----
-- 1 .......1..
-- ------------
-- .....1......

  
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

      showC :: Context -> String
      showC c = let sCtx (x,t) =  x ++ ": " ++ show t in mconcat $ sCtx <$> c

      showJ :: Judgement -> String
      showJ (cx,n,t) = showC cx ++ " |- " ++ show n ++ " : " ++ show t

      showL :: Int -> Int -> Int -> String
      showL l m r = replicate l ' ' ++ replicate m '-' ++ replicate r ' '
      
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
        | k <= l+m+r = (ll,k,rr, (replicate ll ' ' ++ x ++ replicate rr ' ')
                                 : showL ll k rr
                                 : xs)
        | otherwise  = (0,k,0, x
                               : replicate k '-'
                               : [ replicate (-ll) ' '
                                   ++ y
                                   ++ replicate (-rr) ' '
                                 | y <- xs
                                 ])
        where
          k = length x
          i = div (m - k) 2
          ll = l+i
          rr = r+m-k-i

      extend :: Int -> [String] -> [String]
      extend i strs = strs ++ repeat (replicate i ' ')

      sidebyside :: (Int,Int,Int,[String]) -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
      sidebyside (l1,m1,r1,d1) (l2,m2,r2,d2)
        | length d1 > length d2 = ( l1 , m1+r1+2+l2+m2 , r2 , [ x ++ "  " ++ y | (x,y) <- zip d1 (extend (l2+m2+r2) d2)])
        | otherwise             = ( l1 , m1+r1+2+l2+m2 , r2 , [ x ++ "  " ++ y | (x,y) <- zip (extend (l1+m1+r1) d1) d2])

type Term = Tm

derive :: Term -> Derivation
derive term =
  let
    emptyCtx :: Context
    emptyCtx = []

    simpleT :: T
    simpleT = TConst [T Ho "a"] :=> TConst [T Ho "a"]
    
  in
    case term of
      St -> Star (emptyCtx, term, simpleT)
      V x St -> Variable (emptyCtx, term, simpleT)
      V x t  -> Fusion (emptyCtx, term, simpleT)
                       (derive (V x St))
                       (derive t)
      B x t lo St -> Abstraction  (emptyCtx, term, simpleT)
                                  (derive (V x St))
      B x t lo t' -> Fusion (emptyCtx, term, simpleT)
                            (derive (B x t lo St))
                            (derive t')
      P t lo St -> Application (emptyCtx, term, simpleT)
                               (derive t)
      P t lo t' -> Fusion (emptyCtx, term, simpleT)
                          (derive (P t lo St))
                          (derive t')               

ex0 = derive (parseFMC "*")
ex1 = derive (parseFMC "x.*")
ex2 = derive (parseFMC "<x:a>.<x:a>.*")
ex3 = derive (parseFMC "[x.*].*")
