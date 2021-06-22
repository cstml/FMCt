W-module Parsing
    (
      parseFMC
    )
where

import Evaluator
import Data.String (IsString(..))
import Syntax
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec

parseFMC :: Parser Tm
parseFMC = undefined

term :: Parser Tm
term = choice [variable, application, abstraction, star]

abstraction :: Parser Tm
abstraction = do
  l <- location
  x <- between (char '<') (char '>') term
  return $ B 

application :: Parser Tm
application = error "undefined"
              
variable :: Parser Tm
variable = do
  x <- (many1 alpha) <> (many alphaNumeric)
  sepparator
  return $ V x St

star :: Parser Tm
star = do
  (eof >> return St)
    <|> (char '*' >> return St)

location :: Parser Lo
location =
  (string "out" >> return Out)
  <|> (string "in" >> return In)

termType :: Parser T

--------------------------------------------------------------------------------
-- Aux
sepparator :: Parser ()
sepparator = eof <|> (between spaces spaces (oneOf ".;") >> return ())

alpha :: Parser Char
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

numeric :: Parser Char
numeric = oneOf ['0' .. '9']

alphaNumeric :: Parser Char
alphaNumeric = alpha <|> numeric

operators :: Parser Char
operators = oneOf "+-/%=!?"
  
{-
--------------------------------------------------------------------------------
-- Aux 
pSpaces :: Parser ()
pSpaces = do many $ oneOf " .;"
             return ()

pText :: Parser String
pText =  many $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0' .. '9'] ++ "+"
  
--------------------------------------------------------------------------------
-- | Type Parser
pType :: Parser TT
pType = do char ':' >> spaces
           x <- between (char '(' >> spaces) (spaces >> char ')') pType'                      
           return x
           
pType' =  try pVectorType
          <|> try pLocationType
          <|> try pConstantType
--          <|> try pMachineType

-- :(Int)
pConstantType :: Parser TT
pConstantType = do
  x <-  try $ string "*"
        <|> string " " <|> string "_"
        <|> pText
  case x of
    "*" -> return $ CT Star         -- Star
    ""  -> return $ fromString "_"  -- To Be Inferred
    "_" -> return $ fromString "_"  -- To Be Inferred   
    _   -> return $ fromString x    -- Simple Type
-}
