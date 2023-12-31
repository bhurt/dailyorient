{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- mkYesodDispatch defines orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main(
    main
) where

    import           Domain
    import           Routes
    import           Yesod

    getHomeR :: Handler Html
    getHomeR = defaultLayout [whamlet|Hello World!|]

    mkYesodDispatch domainName routes

    main :: IO ()
    main = warp 3000 Domain

