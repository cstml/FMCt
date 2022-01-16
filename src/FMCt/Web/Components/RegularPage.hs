module FMCt.Web.Components.RegularPage (regularPage) where

--import qualified Data.Text.Lazy as LA

import Data.String (IsString (..))
import FMCt.Web.Style.StdStylingHeader (stdStylingHeader)
import qualified Lucid as LU
import qualified Lucid.Bootstrap as LB

regularPage ::
    (LU.ToHtml b, Show b, IsString b) =>
    -- | Subtitle
    b ->
    LU.Html () ->
    LU.Html ()
regularPage st rest =
    let pTitle = LU.h1_ $ LU.a_ [LU.href_ "/"] "FMCt-Web"
        pSubTitle = LU.h2_ . LU.toHtml
        pStyling = stdStylingHeader . (fromString . show)
     in LB.containerFluid_
            $ LU.div_ [LU.name_ "Page Components"]
            $ do
                pStyling st -- add the header to the file containing it's title and css
                LB.span1_ pTitle -- a page title
                LB.span1_ $ pSubTitle st -- a page subtitle
                rest
