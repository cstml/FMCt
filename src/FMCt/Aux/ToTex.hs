{-#LANGUAGE TypeSynonymInstances#-}
{-#LANGUAGE FlexibleInstances#-}

module FMCt.Aux.ToTex where

import System.IO (writeFile)
import FMCt.TypeChecker2
import FMCt.Parsing (parseFMC)
import FMCt.Syntax

writeLocally :: String -> String -> IO ()
writeLocally title str =
  writeFile ("../../deliverables/02-dissertation/tex/files/diagrams/"
             ++ title
             ++ ".tex" ) str

-- | Saves diagram locally to be used in dissertation
saveDiagram :: String -- ^ Diagram name 
            -> String -- ^ Term to be Parsed 
            -> IO ()
saveDiagram diagName term = either (print .show) (writeLocally diagName  . _math . toTex) $ (derive2 . parseFMC) term

_term :: String -> String 
_term x = "\\term{ " ++ x ++ " } "

_type :: String -> String
_type x = "{\\color{blue} " ++ x ++ " } "

_math :: String -> String
_math x = "$$ \n" ++ x ++ "\n $$ \n"

_dfrac :: String -> String -> String
_dfrac t b= '\\' : "dfrac{" ++ t ++ "}{" ++ b ++ "}"

_dfrac2 :: String -> String -> String -> String
_dfrac2 l r b = "\\dfrac{ " ++ l ++ " \\hspace{1cm} " ++ r ++ "}{" ++ b ++ "}"


class ToTex a where
  toTex :: a -> String

instance ToTex Lo where
  toTex = _term . \case
    La -> " \\lambda "
    Out -> "out "
    In -> "in "
    Rnd -> "rnd "
    Nd -> "nd "
    Ho -> " \\gamma "
    Lo x -> x ++ " "

instance ToTex Tm  where
  toTex = \case
    St -> _term "*"
    V bi n ->  _term bi ++ _term " ; " ++ toTex n
    P t lo nt ->
      if lo /= La
      then _term " [ " ++ toTex t ++ _term " ] " ++ toTex lo ++  _term " ; " ++ toTex nt
      else _term " [ " ++ toTex t ++ _term " ] " ++  _term " ; " ++ toTex nt    
    B bi ty lo nt ->
      if lo /= La
      then toTex lo ++ _term " < " ++ _term bi ++ _term " : " ++ (_type . toTex) ty ++ _term ">" ++ _term " ; " ++ toTex nt
      else _term " < " ++ _term bi ++ _term " : " ++ (_type . toTex) ty ++ _term ">" ++ _term " ; " ++ toTex nt
    
instance ToTex T  where
  toTex = \case
    TEmp        -> " \\epsilon "
    TCon x      -> "  " ++ x ++ " "
    TVar x      -> "  " ++ x ++ " "
    TVec []     -> "  "
    TVec [x]    -> toTex x
    TVec (x:xs) -> " ( " ++ toTex x ++ ", " ++ toTex (TVec xs) ++ " ) "
    TLoc l t    -> toTex l ++ "( " ++ toTex t ++ " )"
    t1 :=> t2   -> toTex t1 ++ " \\To " ++ toTex t2

instance ToTex Context  where
  toTex _ = "\\Gamma "

instance ToTex Judgement where
  toTex (ctx, term, t) = toTex ctx ++ "\\vdash " ++ toTex term ++ ":" ++ _type (toTex t)

instance ToTex Derivation where
  toTex = \case
    Star j -> _dfrac mempty (toTex j)
    Variable j d ->  _dfrac (toTex d) (toTex j)
    Abstraction j d -> _dfrac (toTex d) (toTex j)
    Application j p d -> _dfrac2 (toTex p) (toTex d) (toTex j)
