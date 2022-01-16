{-# LANGUAGE TemplateHaskell #-}

module FMCt.Evaluator
    ( -- ** Types.
      Binds,
      Memory,
      State,
      EvalState (..),

      -- ** Lens.
      memory,
      binds,

      -- ** Evaluator.
      eval,
    )
where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Default
import Data.Map as M
import Data.Maybe
import FMCt.Syntax

-- | Memory is a Map between a location and a list of Terms.
type Memory = M.Map Lo [Tm]

-- | Binds are a Map of Tms referring to a list of Terms.
type Binds = M.Map Vv Tm

-- | FMCt State is formed from a tuple of Memory and Binds
data EvalState = EvalState
    { _memory :: Memory,
      _binds :: Binds
    }
    deriving (Show, Eq)

makeLenses ''EvalState

instance Default EvalState where
    def = EvalState M.empty M.empty

-- | Evaluation Environment.
newtype Env = Env ()
    deriving (Show, Eq)

instance Default Env where
    def = Env ()

-- | Evaluator Type.
type Evaluator a = ReaderT Env (State EvalState) a

-- | Pushes a term to a location in the memory.
push :: Tm -> Lo -> Evaluator ()
push t l = do
    s <- lift get
    case s ^. memory . at l of
        Nothing -> lift . put $ s & memory . at l ?~ [t]
        Just _ -> lift . put $ s & memory . at l . _Just %~ (t :)

-- | Pop a term from a location. If the location is empty return Star.
pop :: Lo -> Evaluator Tm
pop l = do
    s <- lift get
    let e = fromMaybe St (s ^? memory . at l . _Just . _head)
    lift . put $
        s & memory . at l . _Just
            %~ ( \case
                     [] -> []
                     _ : xs -> xs
               )
    return e

-- | Pop the term from the location and bind it to the new term
bind :: Vv -> Lo -> Evaluator ()
bind b l = do
    t <- pop l
    m <- lift get
    lift . put $ m & binds . at b ?~ t
    pure ()

-- Replace the term with its bound term.
replace :: Vv -> Tm -> Evaluator Tm
replace b t = do
    m <- lift get
    let bT = fromMaybe St $ m ^. binds . at b
    if bT == St
        then pure t
        else case t of
            St -> pure St
            V b' t' ->
                if b == b'
                    then (bT <>) <$> replace b t'
                    else V b' <$> replace b t'
            P pt l t' -> P <$> replace b pt <*> pure l <*> replace b t'
            B b' ty l t' ->
                if b == b'
                    then error "BINDER OVERRIDE"
                    else B b' ty l <$> replace b t'

-- Interpret a term.
interpret :: Tm -> Evaluator ()
interpret tm = case tm of
    St -> pure ()
    P t lo t' -> push t lo >> interpret t'
    V b t' -> do
        tm' <- replace b t'
        interpret tm'
    B b _ l t' -> bind b l >> interpret t'

-- Evaluate a term.
eval :: Tm -> EvalState
eval = flip execState def . flip runReaderT def . interpret
