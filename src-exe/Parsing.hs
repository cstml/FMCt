{-# LANGUAGE OverloadedStrings #-}

module Parsing () where

import Data.String (IsString(..))
import Syntax
import Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------
-- Aux 
pSpaces :: Parser ()
pSpaces = do many $ oneOf " .;"
             return ()

pText :: Parser String
pText =  many $ oneOf $ ['a'..'z'] ++ ['A'..'Z']
  
--------------------------------------------------------------------------------
-- | Type Parser
pType :: Parser TT
pType = do char ':' >> spaces >> char '(' >> spaces
           x <- pType'
           spaces >> char ')'
           return x
           
pType' =  try pVectorType
          <|> try pLocationType
          <|> try pConstantType 

-- :(Int)
pConstantType :: Parser TT
pConstantType = do
  char '(' >> spaces   
  x <-  try $ string "*"
        <|> string " " <|> string "_"
        <|> pText
  pSpaces >> char ')'
  case x of
    "*" -> return $ CT Star         -- Star
    ""  -> return $ fromString "_"  -- To Be Inferred
    "_" -> return $ fromString "_"  -- To Be Inferred   
    _   -> return $ fromString x    -- Simple Type

-- :(Int)a
pLocationType :: Parser TT
pLocationType = do
  CT x <- pConstantType
  l    <- pText
  return $ VT (fromString l) x

-- : (Int)a :-> (Int)b
pVectorType :: Parser TT
pVectorType = do
  x <- try pLocationType
       <|> try pConstantType
  spaces >> string ":->" >> spaces
  y <- pType'
  return $ x :-> y

--------------------------------------------------------------------------------
-- | Machine Type Parser
pMachineType :: Parser MT
pMachineType = do
  char ':' >> spaces >> char '('
  x <- pType'
  spaces >> string ":=>" >> spaces
  y <- pType'
  spaces >> char ')'
  return $ x :=> y

--------------------------------------------------------------------------------
--xs
pApp :: Parser Tm
pApp = do
  char '['
  t <- pTerm
  char ']'
  l <- pLoc
  return $ Ap t l St

-- "$a<a:t>"
pAbs :: Parser Tm
pAbs = do
  l <- pLoc
  char '<'
  v <- pText
  spaces
  t <- pType
  spaces
  char '>'
  return $ (Ab v t l St)

-- "a"
pLoc :: Parser Lo
pLoc = do
  b <- pText
  return $ (fromString b)
  
-- x
pVar :: Parser Tm
pVar = do
--  b  <- char '@' >> pText
  b  <- pSpaces >> pText
  return $ Va b St

-- | FMC Term Parser
pTerm :: Parser Tm
pTerm = pVar
  <|> pAbs
  <|> do eof >> return St

-- | Expression parser
-- | Concatenates terms into a FMC expression
pExpr :: Parser [Tm]
pExpr  = do
  ( eof >> return [St] ) -- eof return list
    <|> do x <- pTerm      -- otherwise chain expressions
           spaces
           y <- pExpr
           return $  x : y

-- | Used for testing purposes
main :: String -> IO ()
main str = do
  print str
  case parse pTerm "Parser" str of
    Left  err -> putStrLn $ "Err!"     ++ show err
    Right val -> putStrLn $ "Parsed: " ++ show val
  
----------------------------------------------------
test1 = "a"
test3 = "a<_>:a"
test2 = "[x]out"
testT1 = ":(( * ):-> (Int)out :-> (Int))" -- (*) :-> out(Int) :-> (Int)
testMT1 = ":((Int)in :=> (Int)out)" -- (*) :-> out(Int) :-> (Int)
