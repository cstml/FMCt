module FMCt.Parsing
    ( parseFMC,
      parseType,
      parseFMCtoString,
      parseFMC',
      PError (..),
    )
where

import qualified Control.Exception as E
import Control.Monad (void)
import FMCt.Parsing.Aux
import FMCt.Parsing.Terms
import FMCt.Parsing.Types
import FMCt.Syntax (Lo (..), T, Tm (..), Type (..))
import Text.ParserCombinators.Parsec

-- | Main Parsing Function. (Unsafe)
parseFMC :: String -> Tm
parseFMC x = either (E.throw . PTermErr . show) id $ parse term "FMC Parser" x

-- | Main Parsing Function. (Safe)
parseFMC' :: String -> Either ParseError Tm
parseFMC' x = parse term "FMCParser" x

-- | Utility Parsing Function used for the FMCt-Web.
parseFMCtoString :: String -> String
parseFMCtoString x = either show show $ parse term "FMCParser" x
