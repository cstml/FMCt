{-
Main derivator App.
-}
module Main (main) where

import FMCt

main :: IO ()
main = pure ()

-- FIXME
{-
    let inp :: IO (String -> IO ())
        inp = do
            putStrLn "\nEnter: \n1 - for type derivator, \n2 - for legacy type derivator"
            key <- getLine
            case key of
                "1" -> return  print
                "2" -> return  print
                _ -> do
                    putStrLn "Sorry, didn't quite catch that.\nLet's try again.\n"
                    inp
        loop :: (String -> IO ()) -> IO ()
        loop fn = putStr "γ> " >> getLine >>= fn >> loop fn
     in do
            let bLn = replicate 80 '='
            putStrLn $
                mconcat
                    [ bLn,
                      "\n",
                      "Hello, and welcome to the FMCt REPL \n",
                      "May the λ be with you!\n",
                      bLn
                    ]
            inp >>= loop
-}
