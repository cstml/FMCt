{-# LANGUAGE OverloadedStrings #-}

module Parsing
  ( parseFMC
  )where

import Evaluator
import Data.String (IsString(..))
import Syntax
import Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------
-- Aux 
pSpaces :: Parser ()
pSpaces = do many $ oneOf " .;"
             return ()

pText :: Parser String
pText =  many $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0' .. '9'] ++ "+"
  
--------------------------------------------------------------------------------
-- | Type Parser
pType :: Parser TT
pType = do char ':' >> spaces
           x <- between (char '(' >> spaces) (spaces >> char ')') pType'                      
           return x
           
pType' =  try pVectorType
          <|> try pLocationType
          <|> try pConstantType
--          <|> try pMachineType

-- :(Int)
pConstantType :: Parser TT
pConstantType = do
  x <-  try $ string "*"
        <|> string " " <|> string "_"
        <|> pText
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
pMachineType = do x <- spaces >> pType'
                  spaces >> string ":=>" >> spaces
                  y <- spaces >> pType'
                  spaces >> char ')'
                  return $ x :=> y

--------------------------------------------------------------------------------
-- "a"
pLoc :: Parser Lo
pLoc = do
  b <- between spaces spaces pText
  return $ (fromString b)

--[x]loc
pApp :: Parser Tm
pApp = do
  t <- between (char '[') (char ']') pTerm
  l <- pLoc
  return $ Ap t l St

-- "a<a:t>"
pAbs :: Parser Tm
pAbs = try $ do 
  l <- pLoc
  char '<'
  v <- spaces >> pText
  t <- spaces >> pType 
  spaces >> char '>'
  return $ Ab v t l St
  
-- x
pVar :: Parser Tm
pVar = do
  b  <- spaces >> pText
  return $ Va b St

-- | FMC Term Parser
pTerm :: Parser Tm
pTerm =  do eof >> return St
         <|> pApp <|> pAbs  <|> pVar 

pTerms :: Parser [Tm]
pTerms = do eof >> return []
         <|>  do x <- pTerm 
                 y <- pSpaces >> pTerms
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

rStar :: [Tm] -> Tm
rStar [] = St
rStar (x:xs) = case x of
                 Va x St     -> Va x (rStar xs)
                 Ap t l St   -> Ap t l (rStar xs)
                 Ab v t l St -> Ab v t l (rStar xs)

----------------------------------------------------
test1 = "a"
test3 = "a<_>:a"
test2 = "[x]out"
testT1 = ":(( * ):-> (Int)out :-> (Int))" -- (*) :-> out(Int) :-> (Int)
testMT1 = ":((Int)in :=> (Int)out)" -- (*) :-> out(Int) :-> (Int)
test4 = " 2 . 2 . + "
