module FMCt.Parsing (
    parseFMC,
    parseType,
    parseFMCtoString,
    parseFMC',
    PError (..),
) where

import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad (void)
import FMCt.Syntax (Lo (..), T, Tm (..), Type (..))
import Text.ParserCombinators.Parsec

data PError
    = PTermErr String
    | PTypeErr String
    deriving (Show)

instance Exception PError

-- | Main Parsing Function. (Unsafe)
parseFMC :: String -> Tm
parseFMC x = either (E.throw . PTermErr . show) id $ parse term "FMC Parser" x

-- | Main Parsing Function. (Safe)
parseFMC' :: String -> Either ParseError Tm
parseFMC' x = parse term "FMCParser" x

-- | Utility Parsing Function used for the FMCt-Web.
parseFMCtoString :: String -> String
parseFMCtoString x = either show show $ parse term "FMCParser" x

-- | Type Parser.
parseType :: String -> T
parseType x = either (E.throw . PTypeErr . show) id $ parse termType "TypeParser" x

-- | Term Parser.
term :: Parser Tm
term = choice $ try <$> [ application, abstraction, variable, star]

-- | Abstraction Parser.
-- Example: lo<x:a>
abstraction :: Parser Tm
abstraction = do
    l <- location
    v <- char '<' >> spaces >> many1 alpha <> many alphaNumeric
    t <- spaces >> char ':' >> spaces >> absTy <* spaces <* char '>'
    t2 <- (spaces >> sepparator >> spaces >> term) <|> omittedStar
    return $ B v t l t2
      where
        absTy = try higherType <|> try uniqueType
        
application :: Parser Tm
application = do
    t <- between (char '[') (char ']') (term <|> omittedStar)
    l <- location
    t2 <- (spaces >> sepparator >> spaces >> term) <|> omittedStar
    return $ P t l t2

variable :: Parser Tm
variable = do
    x <- spaces >> (many1 alphaNumeric <|> many1 operators)
    t2 <- (spaces >> sepparator >> spaces >> term) <|> omittedStar
    return $ V x t2

star :: Parser Tm
star =  (eof >> return St)
        <|> (void (char '*') >> return St)

omittedStar :: Parser Tm
omittedStar = (string "") >> return St

location :: Parser Lo
location = choice $
  try <$> [ string "out" >> return Out
          , string "in"  >> return In
          , string "rnd" >> return Rnd
          , string "nd"  >> return Nd
          , string "λ"   >> return La
          , string "^"   >> return La
          , string "_"   >> return Ho
          , string "γ"   >> return Ho
          , Lo <$> many1 alphaNumeric
          , string ""    >> return La
          ]

-- |  Type
-- Strings beginning with a small letter 
-- Example:
-- >> a
-- >> b
variableType :: Parser T
variableType = do
    x <- many1 smallCapsAlpha <> many alphaNumeric
    return $ TVar x

-- | Unique Variable type
-- Just an underscore "_"
-- Example: _
uniqueType :: Parser T
uniqueType = do
    _ <- between spaces spaces $ char '_'
    return $ TVar "inferA" :=> TVar "inferB"  -- this gets changed to a unique variable at typecheck time
    -- TODO: preparser that changes these to fresh vars

-- | Constant Type
-- Strings beginning with a capital letter
-- Example: Int, A, B
constantType :: Parser T
constantType = do
    x <- many1 capsAlpha <> many alphaNumeric
    return $ TCon x

-- | Location Types are Types at a specific location
--
-- Examples
-- >> In(Int)
-- >> In(Int=>Int)
locationType :: Parser T
locationType = do
    l <- location
    t <- between (spaces >> char '(') (spaces >> char ')') termType 
    return $ TLoc l t

-- | Vector Types are a list of types.
--
-- Examples
-- >> a,b,c
-- >> a b c
vectorType :: Parser T
vectorType = do
    t <- between
         (spaces >> (char '('))
         (spaces >> (char ')'))
         (termType `sepBy1` (((char ' ') <* spaces) <|> (spaces *> char ',' <* spaces)))
    return $ TVec t

-- | Empty type is empty
--
-- Examples: e => e,  ()=>e
emptyType :: Parser T
emptyType = do
    _ <-  (spaces >> string "e") <|> string "()"
    return $ TEmp

higherType :: Parser T
higherType = do
  --between (char '(') (char ')') $ do 
      t1 <- termType'
      _  <- spaces >> string "=>" >> spaces
      t2 <- termType'
      return $ t1 :=> t2

-- | All Types
termType :: Parser T
termType = try higherType
           <|> try emptyType 
           <|> try vectorType
           <|> try locationType
           <|> try constantType
           <|> try variableType
           <|> try uniqueType

-- | Selected types
termType' :: Parser T
termType' = try vectorType
           <|> try emptyType 
           <|> try locationType
           <|> try constantType
           <|> try variableType



--------------------------------------------------------------------------------
-- Aux
sepparator :: Parser ()
sepparator = eof <|> void (between spaces spaces (oneOf ".;"))

alpha :: Parser Char
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

capsAlpha :: Parser Char
capsAlpha = oneOf $  ['A' .. 'Z']

smallCapsAlpha :: Parser Char
smallCapsAlpha = oneOf $  ['a' .. 'z']

numeric :: Parser Char
numeric = oneOf ['0' .. '9']

alphaNumeric :: Parser Char
alphaNumeric = alpha <|> numeric

operators :: Parser Char
operators = oneOf "+-/%=!?"
