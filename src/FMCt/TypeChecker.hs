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
  = Star 
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

instance Show Derivation where
  show = \case
    Star
      -> pLines [ ((0, 0, 0), ["----------"])
                , ((0, 0, 0), ["|- * : => "])]
    Variable j
      -> pLines [ ((0, 0, 0), [replicate (length (showJudgement j)) '-'])
                , ((0, 0, 0), [showJudgement j])]
    Abstraction judge deriv
      -> pLines [ ((0, 0, 0), [sJudg])
                , ((0, 0, 0), [sDeriv])
                ]
    Fusion judge deriv deriv'
      -> pLines [ 
                ,
                ]
      where
        sDeriv = show deriv
        sJudg = showJudgement judge
    Application judge deriv
      -> show deriv ++ "\n" ++ showJudgement judge
    Fusion judge deriv1 deriv2 -> show deriv1
                                  ++ "  "
                                  ++ show deriv2
                                  ++ "\n"
                                  ++ showJudgement judge
   where
     pLines :: [DisplayLine] -> String
     pLines [] = ""
     pLines (x:xs) = pLine x ++ pLines xs

     pLine :: DisplayLine -> String
     pLine ((l,m,r),c) = case c of
       w:[] -> replicate l ' ' ++ w ++ replicate (m+r) ' ' ++ "\n"
       w:w':[] -> replicate l ' '
                  ++ show c
                  ++ replicate m ' '
                  ++ show w'
                  ++ replicate r ' '
                  ++ "\n"
     
     overline st = replicate (length st) '-' ++ "\n" ++ st

     showJudgement :: Judgement -> String
     showJudgement (c,t,ty) = mconcat [showC c, " |- ", show t, " : ", show ty ]
     
     showC [] = ""
     showC x  = mconcat $ show <$> x

     under :: (Int, [String]) -> String  -> (Int, [String])
     under (lineLength, list) string = (newLength, string : list)
       where
         sLength = length string
         newLength = if sLength > lineLength then sLength else lineLength
     
     sideBySide :: (Int, Int, Int, [String]) -> (Int, Int, Int, [String]) -> (Int, Int, Int, [String])
     sideBySide = undefined

     printStack :: (Int, Int, Int, [String]) -> String
     printStack (_,_,_, [])  = ""
     printStack (l,c,r, (x:y:[]) ) = aux1
       where
         aux1 = mconcat [gap l,lines x ,gap c, lines y, gap r] ++ mconcat [gap l, show x, gap c, show y, gap r]
         lines z = replicate (length z) '-'
         gap x = replicate x ' '
    

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
      St -> Star
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
ex2 = derive (parseFMC "<x:a>.*")
ex3 = derive (parseFMC "[x.*].*")
