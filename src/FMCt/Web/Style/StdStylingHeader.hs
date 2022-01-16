module FMCt.Web.Style.StdStylingHeader
    ( stdStylingHeader,
    )
where

import qualified Data.Text.Lazy as L
import FMCt.Web.Style.BootStrap (bootStrapStyling)
import Lucid

stdStylingHeader ::
    -- | Page Title
    L.Text ->
    Html ()
stdStylingHeader title = head_ $ do
    title_ $ toHtml title
    bootStrapStyling
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/style.css"]
