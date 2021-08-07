{-

-}
module Main (main) where

import Control.Monad (forM_, void)
import Data.String (IsString (..))
import FMCt (eval1, parseFMC, printOutput, printStack)

main :: IO ()
main =
    let break = replicate 80 '='
     in do
            putStrLn $
                break ++ "\n"
                    ++ "Hello, and welcome to the FMCt REPL \n"
                    ++ "May the λ be with you!\n"
                    ++ break

            do
                forM_ (repeat 3) $
                    \_ ->
                        putStr "γ> "
                            >> readLn >>= \unPterm ->
                                (return . parseFMC) unPterm >>= \pTerm ->
                                    (putStrLn . ("Term: " ++) . show) pTerm
                                        >> (return . eval1) pTerm >>= \state ->
                                            (print . ("State: " ++) . show) state
                                                >> (putStrLn . printStack) state
                                                >> putStrLn break
                                                >> (putStrLn . printOutput) state
                                                >> putStrLn break

shortcut :: String -> IO ()
shortcut unPterm =
    (return . parseFMC) unPterm
        >>= \pTerm ->
            (putStrLn . ("Term: " ++) . show) pTerm
                >> (return . eval1) pTerm
                >>= \state ->
                    (print . ("State: " ++) . show) state
                        >> (putStrLn . printStack) state
                        >> (putStrLn . printOutput) state
