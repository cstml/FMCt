{-# LANGUAGE OverloadedStrings #-}

module Parsing (parseT) where

import Syntax (TT(..), Tm(..))
import Text.ParserCombinators.Parsec

pGap :: Parser ()
pGap = do
  many $ oneOf " .;"
  return ()

pText :: Parser String
pText = do
  x <- many $ oneOf $ ['a'..'z'] ++ ['A'..'Z']
  return $ pack x
  
---------------------------------------------
-- ":bO"
pType :: Parser FTyp
pType = do
  char ':' >> spaces
  x <- string "bO"
    <|> string "iN"
    <|> string "sT"
  case x of
    "bO" -> return FBool
    "iN" -> return FInt
    "sT" -> return FStr

-- "$a<a>"
pApp :: Parser FMCT
pApp = do
  l <- pLoc
  char '<'
  v <- pFMCT
  char '>'
  return $ Ap v l

-- "$a"
pLoc :: Parser FMCT
pLoc = do
  char '$'
  x <- pText
  return $ L x

-- "_dsad"
pAtom :: Parser FMCT
pAtom = do
  char '_'
  x <- pText
  return $ A x

-- "@loc : Type=
pVar :: Parser FMCT
pVar = do
  b  <- char '@' >> pText
  tt <- pGap >> pType
  t  <- pGap >> char '=' >> pGap >> pFMCT
  return $ V (A b) t tt

---------------------------------------------------
-- | FMC Term Parser
pFMCT :: Parser FMCT
pFMCT = pVar
        <|> pAtom
        <|> pApp
        <|> do eof >> return S

-- | Expression parser
-- | Concatenates terms into a FMC expression
pExpr :: Parser FMCT
pExpr  = do
  ( eof >> return S)      -- if you reached EOF return Star
    <|> do x <- pFMCT     -- otherwise chain expressions
           spaces
           y <- pExpr
           return $ X x y

-- | Used for testing purposes
main :: String -> IO ()
main str = do
  print str
  case parse pExpr "Parser" str of
    Left  err -> putStrLn $ "Err!"     ++ show err
    Right val -> putStrLn $ "Parsed: " ++ show val

-- | Type Parser 
parseT :: String -> FMCT
parseT x = case parse pExpr "FMCT" x of
  Left  err -> error "Could Not Parse"
  Right val -> val
  
----------------------------------------------------
test1 = "$a"
test3 = "a<_>:a"
test2 = "[x]out"
