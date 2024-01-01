{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Domain (
    Domain(..),
    makeDomain,
    resourcesDomain,
    Widget,
    Handler,
    Route(..)
) where

    import           Server.Yesod.Routes
    import           Yesod

    data Domain = Domain

    mkYesodData domainName routes

    -- Note: this has to be *after* mkYesodData or it won't compile.
    instance Yesod Domain

    makeDomain :: IO Domain
    makeDomain = pure Domain
    
