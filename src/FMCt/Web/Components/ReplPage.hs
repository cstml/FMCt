{-# LANGUAGE ExtendedDefaultRules #-}
module FMCt.Web.Pages.Parse
  ( pParse
  )
where
import FMCt.Parsing ( parseFMCtoString, parseFMC )
import FMCt (eval1, printStack)
import qualified Data.Text.Lazy  as LA
import qualified Lucid           as LU
import qualified Lucid.Bootstrap as LB
import FMCt.Web.Style.StdStylingHeader (stdStylingHeader)
import FMCt.Web.Components.RegularPage (regularPage)
import FMCt.Web.Components.Brick       (brick)
pParse :: LA.Text -> LU.Html ()
pParse term = sReplPage'
  where
    sReplPage' = do 
      regularPage "Parser"
      termForm
      parsedBox
      evaluationBox

    -- |  Form where the term can be inserted. 
    termForm :: LU.Html ()
    termForm = do
      LU.hr_ []
      LU.div_ [LU.name_ "term-div"] $ LU.form_ [LU.action_ "parse" ] $ LU.input_ [LU.type_ "text", LU.name_ "term"]

    -- | Box where the parsed term can be seen. 
    parsedBox  = brick "parsed-div" "Parsed Term" "" parsedTerm 
      where parsedTerm = (LU.toHtml . parseFMCtoString . LA.unpack) term

    -- | Box where the evaluation result can be seen.
    evaluationBox = brick "evaluation-div" "Evaluation Result (Stack)" "" $ evalResult
      where evalResult = (LU.toHtml . printStack . eval1 . parseFMC . LA.unpack) term    
