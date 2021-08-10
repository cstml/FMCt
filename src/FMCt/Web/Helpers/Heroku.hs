module FMCt.Web.Helpers.Heroku (herokuGetPort) where

import Control.Exception (try, SomeException)
import System.Environment (getEnv)
import Text.Read (readMaybe)

-- | Simple Helper that gets the port from Heroku's standard way of making
-- available "$PORT". If the port cannot be read, or the value is not correct,
-- the helper will automatically serve to PORT 8080.
herokuGetPort :: IO Int -- ^ Returns the PORT to be served. 
herokuGetPort = do
  args <- try $ getEnv defaultArg :: IO (Either SomeException String)
  port <- return $ either (const defaultPort) ((maybe defaultPort id) . readMaybe) args
  print $ "Serving to PORT: " ++ show port
  return port 
    where
      defaultPort = 8080 :: Int
      defaultArg  = "PORT"
