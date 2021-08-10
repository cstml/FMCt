module FMCt.Web.Style.MainStyle
  ( mainStylePage
  )
where
import Data.Text.Lazy as L
import Clay
  ( Css
  , element
  , (?)
  , grey
  , background
  , render
  , black
  )

cMyStyle :: Css 
cMyStyle = do
  element "Title" ?
    do background grey
  element "term-form" ?
    do background black
  element "Hello" ?
    do background black


mainStylePage :: L.Text
mainStylePage =  render cMyStyle
