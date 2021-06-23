{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Syntax
import Parsing
import Pretty
import Evaluator
import Examples
import TypeChecker
import Control.Monad
import Data.String (IsString(..))
import qualified System.Environment as Sys
import Web.Scotty
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Text.Lazy(Text)

herokuGetPort :: IO Int
herokuGetPort = do
  args <- (Sys.getEnv "PORT")
  port <- (return . read) args::IO Int
  return port

main = do port <- herokuGetPort
          scotty port $ mconcat [a]
            where
              a = get "/parse/:term" $ do
                term <- param "term"
                html $ mconcat ["<h1>", compute term , "</h1>"]

compute :: Text -> Text
compute x = ( L.fromStrict
            . T.pack
            . run
            . show
            . L.toStrict ) x

run :: String -> String
run = (show . eval1 . parseFMC . reverse . tail . reverse . tail)

main' :: IO ()
main' = 
  let
    break = (take 80 . repeat) '='
  in 
  do port <- herokuGetPort
     print port
     putStrLn $
       break ++ "\n"
       ++ "Hello, and welcome to the FMCt REPL \n"
       ++ "May the λ be with you!\n"
       ++ break
       
     do forM_ (repeat 3) $ \_ ->
          putStr "γ> " >>
          readLn >>= \unPterm ->                    
          (return . parseFMC) unPterm >>= \pTerm ->
          (putStrLn . ("Term: "++) . show ) pTerm >>
          (return.eval1) pTerm >>= \state ->
          (print . ("State: " ++) . show ) state >>
          (putStrLn . printStack) state >> (putStrLn) break >>
          (putStrLn . printOutput) state >> (putStrLn) break

       
       
