module FMCt.Parsing.Location where

import qualified Control.Exception as E
import Control.Monad (void)
import FMCt.Parsing.Aux
import FMCt.Syntax
import Text.ParserCombinators.Parsec

-- | Location Parser
--
-- Example
-- @in
location :: Parser Lo
location = do
  choice $ try <$> (declared <> omitted)
 where
  declared =
    fmap
      (\x -> char '@' >> x)
      [ string "out" >> return Out
      , string "in" >> return In
      , string "rnd" >> return Rnd
      , string "nd" >> return Nd
      , string "λ" >> return La
      , string "^" >> return La
      , string "_" >> return Ho
      , string "γ" >> return Ho
      , Lo <$> many1 alphaNumeric
      ]
  omitted = [string "" >> return La]
