-- | Module used for re-rexporting
module FMCt.Web (
    module X,
    main
) where

import FMCt.Web.MainWebsite as X (mainWebsite)

main = mainWebsite
