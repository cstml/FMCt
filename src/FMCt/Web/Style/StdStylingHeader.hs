module FMCt.Web.Style.StdStylingHeader
  ( stdStylingHeader
  )
where

import Lucid
import qualified Data.Text.Lazy as L
import FMCt.Web.Style.BootStrap (bootStrapStyling)

stdStylingHeader
  :: L.Text -- ^ Page Title 
  -> Html ()
stdStylingHeader title =  head_ $ do
  title_ $ toHtml title
  bootStrapStyling
  link_ [rel_ "stylesheet", type_ "text/css", href_ "/style.css"]
