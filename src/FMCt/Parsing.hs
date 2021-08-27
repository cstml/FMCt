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
term = choice $ try <$> [variable, application, abstraction, star]

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
    x <- spaces >> (many1 alphaNumeric <|> many1 operators)
    t2 <- spaces >> sepparator >> spaces >> term
    return $ V x t2

star :: Parser Tm
star = (eof >> return St) <|> (char '*' >> return St)

location :: Parser Lo
location =
    choice $
        try
            <$> [ string "out" >> return Out
                , string "in" >> return In
                , string "rnd" >> return Rnd
                , string "nd" >> return Nd
                , string "λ" >> return La
                , string "^" >> return La
                , string "_" >> return Ho
                , string "γ" >> return Ho
                , do
                    s <- many1 alphaNumeric
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
typeConstant = many1 capsAlpha <> many alphaNumeric

-- | Variable Type
--
-- Example:
-- >> a
-- >> b
variableType :: Parser T
variableType = do
    x <- many1 nCapsAlpha <> many alphaNumeric
    return $ TVar x

-- | Unique Variable type
--
-- Example:
-- >> _
uniqueType :: Parser T
uniqueType = do
    _ <- char '_'
    return $ TVar "inferA" :=> TVar "inferB"  -- this gets changed to a unique variable at typecheck time
    -- TODO: preparser that changes these to fresh vars

-- | Constant Type
--
-- Example:
-- >> Int
-- >> A
-- >> B
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
    t <- between (spaces >> char '(') (spaces >> char ')') termType 
    return $ TLoc l t

-- | Vector Types are a list of types.
--
-- Examples
-- >> a,b,c
-- >> a b c
vectorType :: Parser T
vectorType = do
    t <-
        between
            (spaces >> (char '('))
            (spaces >> (char ')'))
            (termType `sepBy1` (((char ' ') <* spaces) <|> (spaces *> char ',' <* spaces)))
    return $ TVec t

-- | Empty type is empty
--
-- Examples:
-- e => e
-- " " => e
emptyType :: Parser T
emptyType = do
    _ <- (try $ char 'e') <|> (oneOf " " <*spaces )
    return $ TEmp

-- nestedType :: Parser T
-- nestedType = do
--   l <- location
--   ts <- between (spaces >> char '(' >> spaces) (spaces >> char ')' >> spaces) termType
--   return $ TLoc l ts

higherType :: Parser T
higherType = do
    between (char '(') (char ')') $ do
      t1 <- spaces >> termType 
      _  <- spaces >> string "=>" >> spaces
      t2 <- termType <* spaces
      return $ t1 :=> t2

-- flatHigherType :: Parser T
-- flatHigherType = do
--     ts <- termType `sepBy1` (spaces >> string "=>" >> spaces)
--     return $ foldr1 (:=>) ts

termType :: Parser T
termType = try higherType
           <|> try vectorType
           <|> try locationType
           <|> try constantType
           <|> try variableType
           <|> try uniqueType
           <|> emptyType 

--------------------------------------------------------------------------------
-- Aux
sepparator :: Parser ()
sepparator = eof <|> void (between spaces spaces (oneOf ".;"))

alpha :: Parser Char
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

capsAlpha :: Parser Char
capsAlpha = oneOf $  ['A' .. 'Z']

nCapsAlpha :: Parser Char
nCapsAlpha = oneOf $  ['a' .. 'z']


numeric :: Parser Char
numeric = oneOf ['0' .. '9']

alphaNumeric :: Parser Char
alphaNumeric = alpha <|> numeric

operators :: Parser Char
operators = oneOf "+-/%=!?"
