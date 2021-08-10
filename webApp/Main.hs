module Main (main)  where
import FMCt.Web (mainWebsite)

-- | Start serving the website. 
main :: IO ()
main = mainWebsite
