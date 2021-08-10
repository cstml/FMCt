{-# LANGUAGE ExtendedDefaultRules #-}

module FMCt.Web.Pages.Root (pRoot) where

import FMCt.Web.Components.Brick (brick)
import FMCt.Web.Components.RegularPage (regularPage)
import qualified Lucid as LU
import qualified Lucid.Bootstrap as LB

pRoot :: LU.Html ()
pRoot = sMainPage
  where
    sMainPage :: LU.Html ()
    sMainPage = do
        regularPage "FMCt Homepage" $ do
            brick "version" "V.0.0.1" "" "Welcome to the FMCt Web Interpreter."
            brick "version" "V.0.0.1" "" "Links"
            LB.row_ $ do
                LB.span2_ $
                    LU.div_ [LU.name_ "Links"] $ do
                        LU.h3_ "Links"
                        LU.ul_ $ LU.a_ [LU.href_ "parse?term=*"] "Parser"
                        LU.ul_ $ LU.a_ [LU.href_ "derive?term=*"] "Derivation Tester"
