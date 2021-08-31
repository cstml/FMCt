{-# LANGUAGE ScopedTypeVariables #-}

module FMCt.Web.MainWebsite (mainWebsite, mainWebsitePort) where

import Control.Exception
import Control.Monad.IO.Class
import Data.String (fromString)
import FMCt.TypeChecker (TError (..))
import FMCt.Web.Components.MainPage (mainPage)
import FMCt.Web.Helpers.Heroku (herokuGetPort)
import FMCt.Web.Pages.Derive (pDerive)
import FMCt.Web.Pages.Evaluator (pEvaluator)
import FMCt.Web.Pages.Root (pRoot)
import FMCt.Web.Style.MainStyle (mainStylePage)
import Lucid as LU
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import qualified Web.Scotty as S (get, html, param, scotty)
import Web.Scotty.Trans

mainWebsitePort :: Int -> IO ()
mainWebsitePort port = do
    let rMainPage = get "/" $ (html . LU.renderText) pRoot

    -- Parse is where the term gets Parsed and Evaluated.
    let rParse = get "/parse" $ do
            term <- param "term"
            (html . LU.renderText . pEvaluator) term

    -- Derivation Page.
    let rDerivationPage = get "/derive" $ do
            term <- param "term"
            (html . LU.renderText . pDerive) term

    -- Styling.
    let css = S.get "/style.css" $ html mainStylePage    

    -- Routes that will be served:
    scottyT port id $ do
        rMainPage
        rParse
        css
        rDerivationPage


-- | Start serving the website.
mainWebsite :: IO ()
mainWebsite = do
    port <- herokuGetPort
    mainWebsitePort port
   
