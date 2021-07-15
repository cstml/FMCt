{-|
Module      : Syntax
Description : Syntax module of the FMCt.

Syntax module of the FMCt.
-}
module FMCt.Syntax
  ( LTConstant(..)
  , Lo(..)
  , T
  , TConstant
  , TType
  , TVariable
  , Tm(..)
  , Type(..)
  , Vv
  )
where

-- | Variable Value DataType 
type Vv = String            -- ^ Variable Value is represeted by a String.

-------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
-------------------------------------------

-- | FMC Terms Type
data Tm = V Vv Tm          -- ^ Variable
        | P Tm Lo Tm       -- ^ Application or Push: [M]a.N
        | B Vv T  Lo Tm    -- ^ Abstraction or Pop:  a\<x:t\>.N
--        | E Tm Tm          -- ^ Evaluate Term -- might drop
        | St               -- ^ Star 
        deriving (Eq, Ord)

--------------------------------------------------------------------------
-- Location Parametrised types = out(a), in(a),b,in(c), int, in(a,b,c)) --
--------------------------------------------------------------------------

infixr 9 :=>

-- | Type Constants
type TConstant = String -- ^ Type constants are strings, with void being represented by `""` or empty String.

-- | Type Variables
type TVariable = String  -- ^ Type variables are placeholders that can be replaced with any type.

-- | Location Type Constant
data LTConstant = T Lo TConstant
        deriving (Eq, Ord)

type T = Type LTConstant

-- | Type data structure
data Type a = TConst [a]         -- ^ Type Vector.
            | TLocat Lo (Type a) -- ^ Location Parametrised Type.
            | Type a :=> Type a  -- ^ A FMC Type.
            deriving (Eq, Ord)

-- | Term Types hold either a Type Variable or a Location Type Constant
type TType = Type (Either TVariable LTConstant)

-- | TypedTerms
type TTm = (Tm, TType)

--------------------------------------------
-- Location = {out, in, rnd, nd, x, γ, λ} --
--------------------------------------------
  
-- | Predefined Locations of the FMC together with general locations.
data Lo = Out              -- ^ User Output Location - can only be pushed to.
        | In               -- ^ User Input Location - can only be popped from.
        | Rnd              -- ^ Rnd Input Stream - can only be popped from.
        | Nd               -- ^ Non Deterministic Stream - can only be popped from.
        | Ho               -- ^ Home stack : γ ∈ A.
        | La               -- ^ Default push Location: λ ∈ A.
        | Lo  String       -- ^ any other location: x ∈ A.
        | InInt            -- ^ User Input Location for Ints
        | InBool           -- ^ User Input Location for Bools
        | InChar           -- ^ User Input Location for Chars
        deriving (Eq, Ord)
  
--------------------------------------------------------------------------------
-- Show instances
instance Show Lo where
  show x = case x of
    Out  -> "out"
    In   -> "in"
    Rnd  -> "rnd"
    Nd   -> "nd"
    Ho   -> "γ"
    La   -> "λ"
    Lo y -> y

instance (Show a) => Show (Type a) where
  show x = case x of
    TConst y -> show y
    TLocat l y -> show l ++ "(" ++ show y ++ ")"
    t1 :=> t2 -> mconcat ["(", show t1, " => ", show t2, ")"]
    --TVector x -> "(" ++ (mconcat $ (++ ", ") . show <$> x) ++ ")"

instance Show Tm where
  show x = case x of
    B v t l t' -> show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
    P t l t' -> "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
    V v t -> v ++ "." ++ show t -- untyped version
--    V v tt t   -> v ++ ":" ++ show tt ++  "." ++ show t -- typed version
--    E _ _ -> ""
    St          -> "*"

instance Functor Type where
  fmap f term
    = case term of
        TConst x -> TConst $ f <$> x
        TLocat l x -> TLocat l $ f <$> x
        a :=> b -> (f <$> a) :=> (f <$> b)

instance Show LTConstant where
  show (T l c) = mconcat [show l, "(", c, ")"]
                  
