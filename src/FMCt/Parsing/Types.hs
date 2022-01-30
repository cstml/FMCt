module FMCt.Parsing.Types where

import qualified Control.Exception as E
import Control.Monad (void)
import FMCt.Parsing.Aux
import FMCt.Parsing.Location
import FMCt.Syntax
import Text.ParserCombinators.Parsec

-- | Type Parser.
parseType :: String -> T
parseType x = either (E.throw . PTypeErr . show) id $ parse termType "TypeParser" x

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
    return $ TVar "inferA" :=> TVar "inferB" -- this gets changed to a unique variable at typecheck time
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
    t <-
        between
            (spaces >> char '(')
            (spaces >> char ')')
            termType
            `sepBy1` ((char ' ' <* spaces) <|> (spaces *> char ',' <* spaces))
    return $ TVec t

-- | Empty type is empty
--
-- Examples: e => e,  ()=>e
emptyType :: Parser T
emptyType = do
    _ <- (spaces >> string "e") <|> string "()"
    return $ TEmp

higherType :: Parser T
higherType = do
    --between (char '(') (char ')') $ do
    t1 <- termType'
    _ <- spaces >> string "=>" >> spaces
    t2 <- termType'
    return $ t1 :=> t2

-- | All Types
termType :: Parser T
termType =
    choice
        [ try higherType
        , try emptyType
        , try vectorType
        , try locationType
        , try constantType
        , try variableType
        , try uniqueType
        ]

-- | Selected types
termType' :: Parser T
termType' =
    choice
        [ try vectorType
        , try emptyType
        , try locationType
        , try constantType
        , try variableType
        ]
