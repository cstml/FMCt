module Syntax
  ( T(..)
  , L(..)
  , TT(..)
  , Tm(..)
  , Vt(..)
  , Vv(..)
  , Lo(..) )
where
import           Data.Monoid    (Monoid, (<>), mempty, mconcat)
import           Data.Semigroup (Semigroup)
import           Data.Traversable
import           Data.Functor
import qualified Data.Map  as M
import           Data.Map  (Map, (!?))

-- | Variable Value Type 
type Vv = String            -- ^ Variable Value

-------------------------------------------
-- M,N  = * | x.N | [M]a.N | a<x : t>. N --
-------------------------------------------

-- | FMC Terms Type
data Tm = V Vv  Tm          -- ^ Variable
        | P Tm Lo Tm       -- ^ Application or Push [M]a.N
        | B Vv TT Lo Tm    -- ^ Abstraction or Pop  a<x:t>.N 
        | St                -- ^ Star 
        deriving (Eq)

-------------------------------------------
--  Simple Types == a | a b c | e          --
-------------------------------------------
-- | Constant Variable types
data Vt = C String          -- ^ a type variable
        | E                 -- ^ the empty - special constant type
        deriving Eq
  
-- | Simple Types 
data T  = T  Vt           -- ^ a constant type
        | TV [T]          -- ^ a list of types
        deriving Eq

--------------------------------------------
-- Location = {out, in, rnd, nd, x, γ, λ} --
--------------------------------------------
-- | Predefined Locations of the FMC together with general locations.
data Lo = Out              -- ^ User Output Location
        | In               -- ^ User Input Location
        | Rnd              -- ^ Rnd Input Stream
        | Nd               -- ^ Non Deterministic Stream
        | Ho               -- ^ Home stack : γ ∈ A
        | La               -- ^ Default push Location:   λ ∈ A
        | Lo  String       -- ^ any other location: x ∈ A
        deriving (Eq, Ord)

-----------------------------------------------------------------------
-- Location Parametrised types = out(a), in(a,b,c), (int, in(a,b,c)) --
-----------------------------------------------------------------------
-- | Location Parametrised Types
newtype L  = L {getMap :: Map Lo T}
  deriving Eq

-- | Type constructor for a FMTt type
infixl 9 :=>
  
-- | FMCt Types
data TT  = TT :=> TT  -- ^ a FMCt        Type
         | WT L       -- ^ a Location    Type 
         | WF TT      -- ^ a nested FMCt Type
         deriving Eq
  
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
    Lo x -> x
  
instance Show Vt where
  show x = case x of
    E    -> "e"
    C a  -> a

instance Show Tm where
  show x = case x of
    B v t l t' -> show l ++ "<" ++ v ++ ":" ++ show t ++ ">" ++ "." ++ show t'
    P t l t'   -> "[" ++ show t ++ "]" ++ show l ++ "." ++ show t'
    V v t      -> v ++ "." ++ show t
    St          -> "*"

instance Show T where
  show x = case x of
    (T v)   -> show v
    (TV t1) -> mconcat $ (++ " ") . show <$>  t1 

instance Show TT where
  show x = case x of
    WT x    -> show x
    WF x    -> "(" ++ show x ++ ")"
    x :=> y -> "(" ++ show x ++ " => " ++ show y ++ ")"

instance Show L where
  show (L x) = mconcat $ (\(x,y) -> show x ++ "(" ++ show y ++ ") ")  <$>  M.toAscList x


