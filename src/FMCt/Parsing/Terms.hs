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
term = choice . fmap try $ [application, abstraction, inferredTypeAbstraction, variable, star] 

-- | Abstraction Parser.
-- Example: lo<x:a>
abstraction :: Parser Tm
abstraction =
  let
    absTy = try higherType <|> try uniqueType
  in  do
    l <- location
    lAbsBrck >> spaces
    v <- binder
    between spaces spaces typeSep
    ty <- absTy <* spaces <* rAbsBrck
    t2 <- (spaces >> separator >> spaces >> term) <|> omittedStar
    return $ B v ty l t2

-- | Abstraction Parser.
-- Example: lo<x:a>
inferredTypeAbstraction :: Parser Tm
inferredTypeAbstraction = do
    l <- location
    lAbsBrck >> spaces
    v <- binder
    let ty = TVar "_"
    t2 <- (spaces >> separator >> spaces >> term) <|> omittedStar
    return $ B v ty l t2
  

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

binder :: Parser String
binder =  many1 alpha <> many alphaNumeric
