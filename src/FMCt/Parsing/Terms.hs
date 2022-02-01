module FMCt.Parsing.Terms where

import qualified Control.Exception as E
import Control.Monad (void)
import FMCt.Parsing.Aux
import FMCt.Parsing.Location
import FMCt.Parsing.Types
import FMCt.Syntax
import Text.ParserCombinators.Parsec

term :: Parser Tm
term = mconcat <$> pTerm `sepEndBy1` (char ';') 

-- | Term Parser.
pTerm :: Parser Tm
pTerm =  choice . fmap try $ 
  [ application
  , abstraction
  , inferredTypeAbstraction
  , variable
  , star
  ]

-- | Abstraction Parser.
-- Example: lo<x:a>;*
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
    return $ B v ty l St

-- | Abstraction Parser.
-- Example: lo<x:a>
inferredTypeAbstraction :: Parser Tm
inferredTypeAbstraction = do
    l <- location
    lAbsBrck >> spaces
    v <- binder
    let ty = TVar "_"
    return $ B v ty l St 
  

application :: Parser Tm
application = do
    t <- between (char '[') (char ']') (term <|> omittedStar)
    l <- location
    return $ P t l St

variable :: Parser Tm
variable = do
    x <- spaces >> (many1 alphaNumeric <|> many1 operators)
    return $ V x St

star :: Parser Tm
star = (eof >> return St) <|> (void (char '*') >> return St)

omittedStar :: Parser Tm
omittedStar = (string "") >> return St

binder :: Parser String
binder =  many1 alpha <> many alphaNumeric
