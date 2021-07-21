module FMCt.Parsing
    ( parseFMC
    , parseType
    , parseFMCtoString
    )
where
import Control.Monad (void)

import FMCt.Syntax (Tm(..), Lo(..), T(..), Type(..))
import Text.ParserCombinators.Parsec

-- | Main Parsing Function.
parseFMC :: String -> Tm
parseFMC x = case parse term "FMCParser" x of
  Right x -> x
  Left  e -> error $ show e

-- | Utility Parsing Function used for the FMCt-Web.
parseFMCtoString :: String -> String
parseFMCtoString x = case parse term "FMCParser" x of
  Right x -> show x
  Left  e -> show e

-- | Type Parser.
parseType :: String -> T
parseType x = case parse termType "TypeParser" x of
  Right x -> x
  Left  e -> error $ show e

-- | Term Parser.
term :: Parser Tm
term = choice [variable, application, abstraction, star]

-- | Abstraction Parser.
--
-- Example:
-- >> <x:a>
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
star = (eof >> return St) <|> (char '*' >> return St)

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

type TConstant = String

-- | Type Constant Parser. Type Constants are simple strings:
--
-- Examples :
-- >> Int
-- >> Bool
typeConstant :: Parser TConstant
typeConstant = many1 alpha <> many alphaNumeric


-- | Home Constants are constants that are on the lambda row
--
-- Examples:
-- >> Int <-> λ(Int)
homeType :: Parser T
homeType = do
  typeC <- between (char '(') (char ')') termType
  return $ TLoc La typeC

-- | Constant Type
--
-- Example:
-- >> Int
-- >> a
-- >> b
constantType :: Parser T
constantType = do
  x <- typeConstant
  return $ TCon x
    
-- | Location Types are Types at a specific location
--
-- Examples
-- >> In(Int)
-- >> In(Int=>Int)
locationType :: Parser T
locationType = do
  l <- location
  t <- between (spaces >> char '(') (spaces >> char ')') (constantType <|> termType <|> constantType)
  return $ TLoc l t

-- | Vector Types are a list of types.
--
-- Examples
-- >> a,b,c
-- >> a b c
vectorType :: Parser T
vectorType = do
  t <- between (spaces >> (char '('))
               (spaces >> (char ')'))
               (termType `sepBy1` (((char ' ') <* spaces) <|> (spaces *> char ',' <* spaces)))
  return $ TVec t 

-- | Empty type is empty
--
-- Examples:
-- e => e
-- "" => ""
emptyType :: Parser T
emptyType = do
    void (char 'e') <|> spaces
    return $ TCon ""

nestedType :: Parser T
nestedType = do
  l <- location
  ts <- between (spaces >> char '(' >> spaces) (spaces >> char ')' >> spaces) termType
  return $ TLoc l ts

higherType :: Parser T
higherType = do
  ts <- between (char '(') (char ')') (termType `sepBy1` (spaces >> string "=>" >> spaces))
  return $ foldr1 (:=>) ts

termType :: Parser T
termType = try higherType <|> try vectorType <|> try locationType <|> try constantType <|> emptyType
           
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
