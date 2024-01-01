{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Server.Yesod.Handler.Home (
    getHomeR
) where

    import           Domain
    import           Yesod

    getHomeR :: Handler Html
    getHomeR = defaultLayout [whamlet|Hello World!|]


