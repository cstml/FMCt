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
        , "3 - for evaluator"
        ]
      key <- getLine
      case key of
        "1" -> forever parser
        "3" -> forever evaluator
        _ -> print "Sorry, didn't quite catch that.\nLet's try again.\n"

parser :: IO Tm
parser =
  let
    console = "γ>"
  in do 
    print console
    result <- parseFMC <$> getLine
    putStrLn . show $ pShow <$> result
    case result of 
      Right tm -> return tm
      Left _ -> parser 

evaluator :: IO ()
evaluator = do 
  result <- eval <$> parser
  putStrLn . show $ result 
