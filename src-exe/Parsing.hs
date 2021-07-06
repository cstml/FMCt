module Parsing
    (
      parseFMC
    )
where
import Control.Monad (void)
import Data.String (IsString(..))
import Syntax 
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec


parseFMC :: String -> Tm
parseFMC x = case parse term "FMCParser" x of
  Right x -> x
  Left  e -> error $ show e

term :: Parser Tm
term = choice [variable, application, abstraction, star]

abstraction :: Parser Tm
abstraction = do
  l <- location
  v <- char '<' >> spaces >> many1 alpha <> many alphaNumeric
  t <- spaces >> char ':' >> termType <* spaces <* char '>'
  t2 <- spaces >> sepparator >> term 
  return $ B v t l t2

application :: Parser Tm
application = do
  t <- between (char '[') (char ']') term
  l <- location
  t2 <- sepparator >> term
  return $ P t l t2
                
variable :: Parser Tm
variable = do
  x <- spaces >> ( many1 alphaNumeric <|> many1 operators )
  t2 <- spaces >> sepparator >> spaces >>  term
  return $ V x t2

star :: Parser Tm
star = do
  (eof >> return St)
    <|> (char '*' >> return St)

location :: Parser Lo
location = 
  choice [ string "out" >> return Out
         , string "in" >> return In
         , string "rnd" >> return Rnd
         , string "nd" >> return Nd
         , string "λ" >> return La
         , string "^" >> return La
         , string "_" >> return Ho
         , string "γ" >> return Ho 
         , do s <- many1 alphaNumeric
              return $ Lo s
         , string "" >> return La
         ]

-- | Type Constants are simple strings:
--
-- Examples :
-- >> Int
-- >> Bool
typeConstant :: Parser TConstant
typeConstant = do
  x <- many1 alpha <> many alphaNumeric
  return x

-- | Home Constants are constants that are on the lambda row
--
-- Examples:
-- >> Int <-> λ(Int)
homeConstant :: Parser TT
homeConstant = do
  x <- typeConstant 
  return $ T La x

-- | Location Types are constants at a specific location
--
-- Examples
-- >> In(Int)
locationConstant :: Parser TT
locationConstant = do
  l <- location
  t <- between (spaces >> char '(') (spaces >> char ')') typeConstant
  return $ T l t

-- | Empty type is empy
--
-- Examples:
-- e => e
-- "" => ""
emptyType :: Parser T
emptyType = do
    void (char 'e') <|> spaces
    return $ TConst []

simpleType :: Parser T
simpleType = do
  do ts <- (homeConstant <|> locationConstant)  `sepBy1` (spaces >> (char ',') >> spaces)
     return $ TConst ts
  <|> emptyType

nestedType :: Parser T
nestedType = do
  l <- location
  ts <- between (spaces>>char '('>>spaces) (spaces >>char ')' >> spaces) termType
  return $ TLocat l ts

higherType :: Parser T
higherType = do
  ts <- between (char '(') (char ')') (termType `sepBy1` (spaces >> (string "=>") >> spaces))
  return $ foldr1 (:=>) ts

termType :: Parser T
termType = higherType <|> try nestedType <|> simpleType
                  
--------------------------------------------------------------------------------
-- Aux
sepparator :: Parser ()
sepparator = eof <|> void (between spaces spaces (oneOf ".;"))

alpha :: Parser Char
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

numeric :: Parser Char
numeric = oneOf ['0' .. '9']

alphaNumeric :: Parser Char
alphaNumeric = alpha <|> numeric

operators :: Parser Char
operators = oneOf "+-/%=!?"

-- parse example
ex1 = parseTest term "x . y . [*]. [*] . <x:((int,bool))>"
ex2 = parseTest term "x . y . [*]. [*] . <x:_>"
ex3 = parseTest term "x . y . [*]. [*] . <x:(a(int,bool) => (int))>"
ex4 = parseTest term "x . y . [*]. [*] . <x:(a(a) => (a) => (b))>"  -- higher order type 
ex5 = parseTest term "x . y . [*]. [*] . <x:(a(ab,b) => (int)), (b(int))>"
