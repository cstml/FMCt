{-# LANGUAGE ExtendedDefaultRules #-}

module FMCt.Web.Components.MainPage (mainPage) where

import qualified Data.Text.Lazy as LA
import FMCt.Web.Components.Brick (brick)
import FMCt.Web.Components.RegularPage (regularPage)
import FMCt.Web.Style.StdStylingHeader (stdStylingHeader)
import qualified Lucid as LU
import qualified Lucid.Bootstrap as LB

mainPage :: LA.Text
mainPage = LU.renderText sMainPage
    where
        sMainPage :: LU.Html ()
        sMainPage = do
            stdStylingHeader "FMCt"
            LB.container_ $ do
                LB.row_ $ do
                    LB.span2_ $ LU.div_ [LU.name_ "Title"] $ LU.h1_ "FMCt-Web"
                    LB.span2_ $ LU.div_ [LU.name_ "SubTitle"] $ LU.h2_ "V.2.0.2.beta"
                    LU.hr_ []
                    LB.span2_ $ LU.h3_ "Welcome to the FMCt Web Interpreter."

                LU.hr_ []
                LB.row_ $ do
                    LB.span2_
                        $ LU.div_ [LU.name_ "Links"]
                        $ do
                            LU.h3_ "Links"
                            --                        LU.ul_ $ LU.a_ [LU.href_ "parse?term=*"] "Parser" -- Broken this is
                            LU.ul_ $ LU.a_ [LU.href_ "derive?term=*"] "Derivation Tester"
