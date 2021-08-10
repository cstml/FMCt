module FMCt.Web.Pages.Derive
  (pDerive)
  where
import qualified Data.Text.Lazy as LA
import qualified Lucid as LU
import qualified Lucid.Bootstrap as LB
import           FMCt.Web.Components.Brick (brick)
import           FMCt.Web.Components.RegularPage (regularPage)
import FMCt (derive)
import FMCt.Parsing (parseFMCtoString, parseFMC')
import Data.List (unfoldr)
pDerive :: LA.Text -> LU.Html ()
pDerive term = sDerivePage
  where
    sDerivePage :: LU.Html ()
    sDerivePage = do 
      regularPage ("Derivator" :: LA.Text) $ LB.span1_ page


    -- | Box for the derivation
    page :: LU.Html ()
    page = do
      LU.body_ $ do
        termForm
        parsedBox
        derivationBox

    -- |  Form where the term can be inserted. 
    termForm :: LU.Html ()
    termForm = do
      LU.hr_ []
      LU.div_ [LU.name_ "term-div"] $ LU.form_ [LU.action_ "derive" ] $ LU.input_ [LU.type_ "text", LU.name_ "term"]

    
    -- | Box where the parsed term can be seen. 
    parsedBox :: LU.Html ()
    parsedBox
      = brick ("parsed-div" :: LA.Text) ("Parsed Term" :: LA.Text) (""::LA.Text) pTerm
      where pTerm = either (LU.toHtml.show) (LU.toHtml.show) $ (parseFMC' . LA.unpack) term
      

    -- | Box where the parsed term can be seen. 
    derivationBox :: LU.Html ()
    derivationBox
      = case (parseFMC' . LA.unpack) term of
          Left e      -> LU.toHtml . show $ e 
          Right pTerm -> brick ("parsed-div"::String) ("Parsed Term"::LA.Text) (""::LA.Text) deriv      
            where deriv = either (rBr . show) (rBr . show) $ derive pTerm
            
    -- | replace newline with Paragraph, and spaces with .
    rBr :: String -> LU.Html ()
    rBr str = aux spStr
      where
        aux = \case 
          [] -> LU.hr_ []
          (x:xs) -> LU.p_ [] (LU.toHtml x) >> aux xs
      
        separateBy :: Eq a => a -> [a] -> [[a]]
        separateBy chr = unfoldr sep where
          sep [] = Nothing
          sep l  = Just . fmap (drop 1) . break (== chr) $ l

        spStr = separateBy '\n' $ replaceSp str
  
        replaceSp [] = []
        replaceSp(' ' : xs) = ' ' : replaceSp xs
        -- space taken from https://qwerty.dev/whitespace/
        replaceSp( x  : xs) =  x  : replaceSp xs    
