module Main (main) where
import FMCt.Syntax
import FMCt.Parsing
import FMCt.Pretty
import FMCt.Evaluator
import FMCt.Examples
import FMCt.TypeChecker(typeCheck)
import Control.Monad(void, forM_)
import Data.String (IsString(..))

main :: IO ()
main =
  let
    break = replicate 80 '=' 
  in 
  do putStrLn $
       break ++ "\n"
       ++ "Hello, and welcome to the FMCt REPL \n"
       ++ "May the λ be with you!\n"
       ++ break
       
     do forM_ (repeat 3) $
          \_ -> putStr "γ> " >>
          readLn >>= \unPterm ->                    
          (return . parseFMC) unPterm >>= \pTerm ->
          (putStrLn . ("Term: "++) . show ) pTerm >>
          (return.eval1) pTerm >>= \state ->
          (print . ("State: " ++) . show ) state >>
          (putStrLn . printStack) state >> putStrLn break >>
          (putStrLn . printOutput) state >> putStrLn break

shortcut :: String -> IO ()
shortcut unPterm = (return . parseFMC) unPterm >>=
                   \pTerm -> (putStrLn . ("Term: "++) . show ) pTerm >>
                   (return.eval1) pTerm >>=
                   \state -> (print . ("State: " ++) . show ) state >>
                   (putStrLn . printStack) state  >>
                   (putStrLn . printOutput) state

