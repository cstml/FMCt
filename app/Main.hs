{-
Main derivator App.
-}
module Main (main) where

import Control.Monad
import FMCt

main :: IO ()
main =
  let
    welcomeMsg = "Welcome to the FMCt interpreter."
  in
    forever $ do
      putStrLn welcomeMsg
      sequence $ fmap putStrLn 
        [ "Enter:"
        , "1 - for parser,"
        , "2 - for legacy type derivator"
        ]
      key <- getLine
      case key of
        "1" -> parser
        _ -> print "Sorry, didn't quite catch that.\nLet's try again.\n"        

parser :: IO ()
parser =
  let
    console = "γ>"
  in do 
    print console
    result <- parseFMC' <$> getLine
    print $ pShow <$> result 
