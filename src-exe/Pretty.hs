module Pretty
  ( printStack
  , printOutput
  )where
import Syntax
import Evaluator
import Data.Map as M

printStack :: State -> String
printStack (m,b) = "Memory: \n" ++ printer mem ++ "Bindings: \n" ++ printer bin
  where 
    mem = M.toList m
    bin = M.toList b
    printer [] = ""
    printer (x@(l,ts):xs) = show l ++ "[" ++ show ts ++ "]" ++ "\n" ++ printer xs

printOutput (m,b) = "Output: \n" ++ printer out
  where
    out = maybe [] reverse (m M.!? Out)
    printer x = case x of
      [] -> ""
      (x:xs) -> show x ++ "\n" ++ printer xs
    
    

