module Main (main) where
import Syntax
import Parsing
import Pretty
import Evaluator
import Examples
import TypeChecker
import Control.Monad
import Data.String (IsString(..))

main :: IO ()
main = forM_ (repeat 3) $ \_ ->
       readLn >>= \unPterm ->                    
       (return . parseFMC) unPterm >>= \pTerm ->
       (putStrLn . ("Term: "++) . show ) pTerm >>
       (return.eval1) pTerm >>= \state ->
       (print . ("State: " ++) . show ) state >>
       (putStrLn . printStack) state >>
       (putStrLn . printOutput) state 

       
       
