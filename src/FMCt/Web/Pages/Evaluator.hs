module FMCt.Web.Pages.Evaluator (
    pEvaluator,
) where

import qualified Data.Text.Lazy as LA
import FMCt (eval1, parseFMC, parseFMCtoString, printStack)
import FMCt.Web.Components.Brick (brick)
import FMCt.Web.Components.RegularPage (regularPage)
import qualified Lucid as LU

-- | Evaluation page
pEvaluator ::
    -- | Term to be evaluated parsed and evaluated.
    LA.Text ->
    -- | Evaluation Page.
    LU.Html ()
pEvaluator term = sReplPage'
  where
    sReplPage' = do
        regularPage ("Parser" :: LA.Text) $ do
            termForm
            parsedBox
            evaluationBox

    termForm :: LU.Html ()
    termForm = do
        LU.div_ [LU.name_ "term-div"] $ LU.form_ [LU.action_ "parse"] $ LU.input_ [LU.type_ "text", LU.name_ "term"]

    parsedBox =
        brick ("parsed-div" :: LA.Text) ("Parsed Term" :: LA.Text) ("" :: LA.Text) parsedTerm
      where
        parsedTerm = (LU.toHtml . parseFMCtoString . LA.unpack) term

    evaluationBox =
        brick ("evaluation-div" :: LA.Text) ("Evaluation Result (Stack)" :: LA.Text) ("" :: LA.Text) evalResult
      where
        evalResult = (LU.toHtml . printStack . eval1 . parseFMC . LA.unpack) term
