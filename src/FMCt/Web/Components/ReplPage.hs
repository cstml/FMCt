{-# LANGUAGE ExtendedDefaultRules #-}

module FMCt.Web.Pages.Parse
    ( pParse,
    )
where

import qualified Data.Text.Lazy as LA
import FMCt (eval1, printStack)
import FMCt.Parsing (parseFMC, parseFMCtoString)
import FMCt.Web.Components.Brick (brick)
import FMCt.Web.Components.RegularPage (regularPage)
import FMCt.Web.Style.StdStylingHeader (stdStylingHeader)
import qualified Lucid as LU
import qualified Lucid.Bootstrap as LB

pParse :: LA.Text -> LU.Html ()
pParse term = sReplPage'
    where
        sReplPage' = do
            regularPage "Parser"
            termForm
            parsedBox
            evaluationBox
        termForm :: LU.Html ()
        termForm = do
            LU.hr_ []
            LU.div_ [LU.name_ "term-div"] $ LU.form_ [LU.action_ "parse"] $ LU.input_ [LU.type_ "text", LU.name_ "term"]
        parsedBox = brick "parsed-div" "Parsed Term" "" parsedTerm
            where
                parsedTerm = (LU.toHtml . parseFMCtoString . LA.unpack) term
        evaluationBox = brick "evaluation-div" "Evaluation Result (Stack)" "" $ evalResult
            where
                evalResult = (LU.toHtml . printStack . eval1 . parseFMC . LA.unpack) term
