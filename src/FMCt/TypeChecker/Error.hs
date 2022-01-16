module FMCt.TypeChecker.Error where

import Control.Exception

-- | Typechecking Errors.
data TError
    = -- | A Simple, Generic Error.
      ErrSimple String
    | -- | An undefined Type.
      ErrUndefT String
    | -- | A merge Error.
      ErrMerge String
    | -- | Attempting to override declared variable type.
      ErrOverride String
    | -- | Attemptig to use the wrong types.
      ErrWrongT String
    | -- | Not a binder.
      ErrNotBinder String
    | -- | Err arrising at consume level.
      ErrConsume String
    | -- | Err arrising at fuse level.
      ErrFuse String
    deriving (Eq)

instance Show TError where
    show = \case
        ErrSimple s -> "ERR! " ++ s
        ErrUndefT s -> "Undefined Type: " ++ s
        ErrMerge s -> "Cannot Merge: " ++ s
        ErrOverride s -> "Cannot Override: " ++ s
        ErrWrongT s -> "Wrong Type: " ++ s
        ErrNotBinder s -> "Wrong Binder: " ++ s
        ErrConsume s -> "Cannot Consume: " ++ s
        ErrFuse s -> "Cannot Fuse: " ++ s

instance Exception TError
