module FMCt.Parsing.Aux where

import Control.Exception (Exception)
import Control.Monad (void)
import Text.ParserCombinators.Parsec

data PError
    = PTermErr String
    | PTypeErr String
    deriving (Show)

instance Exception PError

separator :: Parser ()
separator = eof <|> void (between spaces spaces (oneOf ".;"))

alpha :: Parser Char
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

capsAlpha :: Parser Char
capsAlpha = oneOf ['A' .. 'Z']

smallCapsAlpha :: Parser Char
smallCapsAlpha = oneOf ['a' .. 'z']

numeric :: Parser Char
numeric = oneOf ['0' .. '9']

alphaNumeric :: Parser Char
alphaNumeric = alpha <|> numeric

operators :: Parser Char
operators = oneOf "+-/%=!?"
