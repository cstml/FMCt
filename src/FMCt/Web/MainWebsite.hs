{-# LANGUAGE ScopedTypeVariables #-}

module FMCt.Web.MainWebsite (mainWebsite) where

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

-- Define a custom exception type.
data Except = Forbidden | NotFound Int | StringEx String | Err SomeException | Err' TError
    deriving (Show)

-- The type must be an instance of 'ScottyError'.
-- 'ScottyError' is essentially a combination of 'Error' and 'Show'.
instance ScottyError Except where
    stringError = StringEx
    showError = fromString . show

-- Handler for uncaught exceptions.
handleEx :: Monad m => Except -> ActionT Except m ()
handleEx Forbidden = do
    status status403
    html "<h1>Scotty Says No</h1>"
handleEx (NotFound i) = do
    status status404
    html $ fromString $ "<h1>Can't find " ++ show i ++ ".</h1>"
handleEx (StringEx s) = do
    status status500
    html $ fromString $ "<h1>" ++ s ++ "</h1>"
handleEx (Err e) = do
    status status500
    html $ fromString $ "<h1>" ++ show e ++ "</h1>"
handleEx (Err' e) = do
    status status500
    html $ fromString $ "<h1>" ++ show e ++ "</h1>"

-- | Start serving the website.
mainWebsite :: IO ()
mainWebsite = do
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
    let css = get "/style.css" $ html mainStylePage

    port <- herokuGetPort -- Fetch the port.

    -- Routes that will be served:
    scottyT port id $ do
        defaultHandler handleEx
        rMainPage
        rParse
        css
        rDerivationPage

bla :: String -> IO String
bla = return
