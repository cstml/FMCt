module FMCt.Web.Pages.Derive (pDerive) where

import Data.List (unfoldr)
import qualified Data.Text.Lazy as LA
import FMCt (derive2, pShow, getTermType, pShow')
import FMCt.Parsing (parseFMC', parseFMCtoString)
import FMCt.Web.Components.Brick (brick)
import FMCt.Web.Components.RegularPage (regularPage)
import qualified Lucid as LU
import qualified Lucid.Bootstrap as LB

pDerive :: LA.Text -> LU.Html ()
pDerive term = sDerivePage
  where
    sDerivePage :: LU.Html ()
    sDerivePage = do
        regularPage ("Derivator" :: LA.Text) $ LB.span1_ page

    page :: LU.Html ()
    page = do
        LU.body_ $ do
            termForm
            parsedBox
            termTypeBox
            derivationBox

    termForm :: LU.Html ()
    termForm = do
        LU.hr_ []
        LU.div_ [LU.name_ "term-div"] $ LU.form_ [LU.action_ "derive"] $ LU.input_ [LU.type_ "text", LU.name_ "term"]

    parsedBox :: LU.Html ()
    parsedBox =
        brick ("parsed-div" :: LA.Text) ("Parsed Term" :: LA.Text) ("" :: LA.Text) pTerm
      where
        pTerm = either (LU.toHtml . show) (LU.toHtml . pShow) $ (parseFMC' . LA.unpack) term

    derivationBox :: LU.Html ()
    derivationBox =
        case (parseFMC' . LA.unpack) term of
            Left e -> LU.toHtml . show $ e
            Right pTerm -> brick ("parsed-div" :: String) ("Term Derivation" :: LA.Text) ("" :: LA.Text) deriv
              where
                deriv = either (rBr . show) (rBr . pShow') $ derive2 pTerm

    termTypeBox :: LU.Html ()
    termTypeBox =
        case (parseFMC' . LA.unpack) term of
            Left e -> LU.toHtml . show $ e
            Right pTerm -> brick ("parsed-div" :: String) ("Term-Type" :: LA.Text) ("" :: LA.Text) deriv
              where
                deriv = case getTermType pTerm of
                  Left e -> show e 
                  Right ty -> pShow pTerm ++ " : "++ pShow ty                                  
                

    rBr :: String -> LU.Html ()
    rBr str = aux spStr
      where
        aux = \case
            [] -> LU.hr_ []
            (x : xs) -> LU.p_ [] (LU.toHtml x) >> aux xs

        separateBy :: Eq a => a -> [a] -> [[a]]
        separateBy chr = unfoldr sep
          where
            sep [] = Nothing
            sep l = Just . fmap (drop 1) . break (== chr) $ l

        spStr = separateBy '\n' $ replaceSp str

        replaceSp [] = []
        replaceSp (' ' : xs) = ' ' : replaceSp xs
        -- space taken from https://qwerty.dev/whitespace/
        replaceSp (x : xs) = x : replaceSp xs
