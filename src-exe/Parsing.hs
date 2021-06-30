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
  
typeConstant :: Parser T
typeConstant = do
  x <- many alpha <> many alphaNumeric
  return $ TConst x
  
vectorType :: Parser T
vectorType = do
  lst <- between (char '(') (char ')') (termType `sepBy1` (spaces >> (char ',') >> spaces))
  return $ TVector lst

locationType :: Parser T
locationType = do
  l <- location
  t <- between (spaces >> char '(') (spaces >> char ')') termType
  return $ TLocat l t

emptyType :: Parser T
emptyType = do
    char 'e' 
    return $ TConst ""

higherType :: Parser T
higherType =  do
  lst <- between (char '(') (char ')') (termType `sepBy1` (spaces >> (string "=>") >> spaces))
  return $ foldr1 (:=>) lst
    
termType :: Parser T
termType = try higherType <|> try locationType <|> vectorType <|> typeConstant
                  
  --choice [sepBy1 (between (spaces >> char '(') (spaces >> char ')') higherType)
--                          (between spaces spaces (char ','))
--                  , spaces >> char '_' >> return []] -- no Type 

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
