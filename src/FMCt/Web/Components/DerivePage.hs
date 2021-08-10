module FMCt.Web.Components.DerivePage
  (derivePage)
  where
import qualified Data.Text.Lazy as L
import Lucid
import Lucid.Bootstrap
import FMCt (evalToString, eval1, printStack)
import FMCt.Web.Style.StdStylingHeader (stdStylingHeader)
import FMCt.TypeChecker (derive)
import FMCt.Parsing (parseFMCtoString, parseFMC )
import Data.List (unfoldr)
derivePage :: L.Text -> L.Text
derivePage term =  renderText sDerivePage
  where
    sDerivePage :: Html ()
    sDerivePage = containerFluid_ $ deriveStyling >> span1_ deriveTitle >> span1_ deriveSubTitle >> span1_ page
    deriveStyling  = stdStylingHeader "FMCt Derivation"
    deriveTitle    = h1_ $ a_ [href_ "/" ] "FMCt-Web"
    deriveSubTitle = h2_ "Derivation"

    -- | Box for the derivation
    page :: Html ()
    page = do
      body_ $ do
        termForm
        parsedBox
        derivationBox

    -- |  Form where the term can be inserted. 
    termForm :: Html ()
    termForm = do
      hr_ []
      div_ [name_ "term-div"] $ form_ [action_ "derive" ] $ input_ [type_ "text", name_ "term"]

    -- | Box where the parsed term can be seen. 
    parsedBox :: Html ()
    parsedBox = do
      hr_ []
      div_ [name_ "parsed-div"] $ do
          h2_ "Parsed Term"
          p_ [name_ "term-parsed"] $ (toHtml . parseFMCtoString . L.unpack) term
      

    -- | Box where the parsed term can be seen. 
    derivationBox :: Html ()
    derivationBox = do
      let parsedT = (parseFMC . L.unpack) term
      hr_ []
      div_ [name_ "parsed-div"] $ do
          h2_ "Parsed Term"
          p_ [name_ "term-parsed"] $ (rBr . show . derive ) parsedT
--      div_ [name_ "parsed-div"] $ do
--          h2_ "Evaluation of Term"
--          p_ [name_ "term-parsed"] $ (toHtml . printStack . eval1 ) parsedT
            

-- | replace newline with Paragraph, and spaces with .
rBr :: String -> Html ()
rBr str = aux spStr
  where
    aux x = case x of
      [] -> hr_ []
      (x:xs) -> do p_ [] $ toHtml x
                   aux xs
      
    separateBy :: Eq a => a -> [a] -> [[a]]
    separateBy chr = unfoldr sep where
      sep [] = Nothing
      sep l  = Just . fmap (drop 1) . break (== chr) $ l

    spStr = separateBy '\n' $ replaceSp str

    replaceSp [] = []
    replaceSp(' ' : xs) = ' ' : replaceSp xs
    -- space taken from https://qwerty.dev/whitespace/
    replaceSp( x  : xs) =  x  : replaceSp xs
    
--rBr (x : xs)
--  | x == '\n' = "<br>" ++ rBr xs
--  | otherwise = x : rBr xs
  

