module FMCt.Parsing.Language where

import Control.Monad (void)
import FMCt.Parsing.Aux
import FMCt.Parsing.Location
import FMCt.Parsing.Types
import FMCt.Syntax

import Text.Parsec.Language
import Text.Parsec
import Text.Parsec.Token
import Data.Functor.Identity
