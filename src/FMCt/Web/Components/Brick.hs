{-# LANGUAGE ExtendedDefaultRules #-}

module FMCt.Web.Components.Brick (brick) where

import Data.String (IsString (..))
import qualified Lucid as LU
import qualified Lucid.Bootstrap as LB

-- | The main component used for the website.
brick ::
    ( IsString a
    , Show a
    , LU.ToHtml a
    , IsString b
    , Show b
    , LU.ToHtml b
    , IsString c
    , Show c
    , LU.ToHtml c
    , IsString d
    , Show d
    , LU.ToHtml d
    ) =>
    -- | Component Name
    a ->
    -- | Title
    b ->
    -- | SubTitle
    c ->
    -- | Content
    d ->
    -- | Brick Component
    LU.Html ()
brick cn ti st co = do
    LU.div_ [(LU.name_ . fromString . show) cn] $
        LB.row_ $ do
            LB.span2_ $ LU.div_ [LU.name_ "Title"] $ (LU.h1_ . LU.toHtml) ti
            LB.span2_ $ LU.div_ [LU.name_ "SubTitle"] $ (LU.h2_ . LU.toHtml) st
            LU.hr_ []
            LB.span2_ $ LU.div_ [LU.name_ "Content"] $ (LU.h3_ . LU.toHtml) co
            LU.hr_ []
