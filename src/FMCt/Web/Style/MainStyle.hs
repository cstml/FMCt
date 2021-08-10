module FMCt.Web.Style.MainStyle (
    mainStylePage,
) where

import Clay (
    Css,
    background,
    black,
    element,
    grey,
    render,
    (?),
 )
import Data.Text.Lazy as L

cMyStyle :: Css
cMyStyle = do
    element "Title"
        ? do background grey
    element "term-form"
        ? do background black
    element "Hello"
        ? do background black

mainStylePage :: L.Text
mainStylePage = render cMyStyle
