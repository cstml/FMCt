module FMCt.Web.Pages.Evaluator
  ( pEvaluator
  )
where
import           FMCt (eval1, printStack,  parseFMCtoString, parseFMC )
import           FMCt.Web.Components.Brick (brick)
import           FMCt.Web.Components.RegularPage (regularPage)
import qualified Data.Text.Lazy  as LA
import qualified Lucid           as LU

-- | Evaluation page 
pEvaluator
  :: LA.Text    -- ^ Term to be evaluated parsed and evaluated.
  -> LU.Html () -- ^ Evaluation Page.
pEvaluator term = sReplPage'
  where
    sReplPage' = do 
      regularPage ("Parser" :: LA.Text) $ do
        termForm
        parsedBox
        evaluationBox

    -- |  Form where the term can be inserted. 
    termForm :: LU.Html ()
    termForm = do
      LU.div_ [LU.name_ "term-div"] $ LU.form_ [LU.action_ "parse" ] $ LU.input_ [LU.type_ "text", LU.name_ "term"]

    -- | Box where the parsed term can be seen. 
    parsedBox
      = brick ("parsed-div"::LA.Text) ("Parsed Term"::LA.Text) (""::LA.Text) parsedTerm 
      where parsedTerm = (LU.toHtml . parseFMCtoString . LA.unpack) term

    -- | Box where the evaluation result can be seen.
    evaluationBox
      = brick ("evaluation-div"::LA.Text) ("Evaluation Result (Stack)"::LA.Text) (""::LA.Text) evalResult
      where evalResult = (LU.toHtml . printStack . eval1 . parseFMC . LA.unpack) term    
