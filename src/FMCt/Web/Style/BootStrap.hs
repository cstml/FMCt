module FMCt.Web.Style.BootStrap (
    bootStrapStyling,
) where

import Lucid

bootStrapStyling :: Html ()
bootStrapStyling = do
    link_
        [ href_ "https://stackpath.bootstrapcdn.com/bootstrap/5.0.0-alpha1/css/bootstrap.min.css"
        , rel_ "stylesheet"
        , integrity_ "sha384-r4NyP46KrjDleawBgD5tp8Y7UzmLA05oM1iAEQ17CSuDqnUK2+k9luXQOfXJCJ4I"
        , crossorigin_ "anonymous"
        ]
