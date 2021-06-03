{-# LANGUAGE OverloadedStrings #-}

module Parsing
  ( parseFMC
  )where

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
pType = do try $ char ':' >> spaces >> char '(' >> spaces
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
pMachineType = do  char ':' >> spaces >> char '('
                   x <- pType'
                   spaces >> string ":=>" >> spaces
                   y <- pType'
                   spaces >> char ')'
                   return $ x :=> y

--------------------------------------------------------------------------------
-- "a"
pLoc :: Parser Lo
pLoc = do
  b <- pText
  return $ (fromString b)

--xs
pApp :: Parser Tm
pApp = do
  char '['
  t <- pTerm
  char ']'
  l <- pLoc
  return $ Ap t l St

-- "a<a:t>"
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
  
-- x
pVar :: Parser Tm
pVar = do
--  b  <- char '@' >> pText
  pSpaces
  b  <- pText
  return $ Va b St

-- | FMC Term Parser
pTerm :: Parser Tm
pTerm = try pAbs
        <|> try pAbs
        <|> try pVar
        <|> do eof >> return St

pTerms :: Parser [Tm]
pTerms = do eof >> return []
         <|>  do x <- pTerm
                 y <- pTerms
                 return $ x : y
         

-- | Used for testing purposes
main :: String -> IO ()
main str = do
  print str
  case parse pTerm "Parser" str of
    Left  err -> putStrLn $ "Err!"     ++ show err
    Right val -> putStrLn $ "Parsed: " ++ show val

parseFMC :: String -> [Tm]
parseFMC x = case parse pTerms "Parser" x of
  Left err -> error $ "Err!"     ++ show err
  Right v  -> v
  
----------------------------------------------------
test1 = "a"
test3 = "a<_>:a"
test2 = "[x]out"
testT1 = ":(( * ):-> (Int)out :-> (Int))" -- (*) :-> out(Int) :-> (Int)
testMT1 = ":((Int)in :=> (Int)out)" -- (*) :-> out(Int) :-> (Int)
