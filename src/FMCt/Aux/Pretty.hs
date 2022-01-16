module FMCt.Aux.Pretty (Pretty (..)) where

class Pretty a where
    pShow :: a -> String
