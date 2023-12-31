{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Routes (
    domainName,
    routes
) where

    import           Yesod
    import           Yesod.Routes.TH.Types

    domainName :: String
    domainName = "Domain"

    routes :: [ ResourceTree String ]
    routes = [parseRoutes|
                / HomeR GET
             |]

