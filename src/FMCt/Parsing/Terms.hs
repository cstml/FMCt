module FMCt.Parsing.Terms where

import qualified Control.Exception as E
import Control.Monad (void)
import FMCt.Parsing.Aux
import FMCt.Parsing.Location
import FMCt.Parsing.Types
import FMCt.Syntax
import Text.ParserCombinators.Parsec

-- | Term Parser.
term :: Parser Tm
term = choice $ try <$> [application, abstraction, variable, star]

-- | Abstraction Parser.
-- Example: lo<x:a>
abstraction :: Parser Tm
abstraction = do
    l <- location
    v <- char '<' >> spaces >> many1 alpha <> many alphaNumeric
    t <- spaces >> char ':' >> spaces >> absTy <* spaces <* char '>'
    t2 <- (spaces >> separator >> spaces >> term) <|> omittedStar
    return $ B v t l t2
  where
    absTy = try higherType <|> try uniqueType

application :: Parser Tm
application = do
    t <- between (char '[') (char ']') (term <|> omittedStar)
    l <- location
    t2 <- (spaces >> separator >> spaces >> term) <|> omittedStar
    return $ P t l t2

variable :: Parser Tm
variable = do
    x <- spaces >> (many1 alphaNumeric <|> many1 operators)
    t2 <- (spaces >> separator >> spaces >> term) <|> omittedStar
    return $ V x t2

star :: Parser Tm
star =
    (eof >> return St)
        <|> (void (char '*') >> return St)

omittedStar :: Parser Tm
omittedStar = (string "") >> return St
