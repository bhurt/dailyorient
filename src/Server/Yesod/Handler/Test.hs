{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Test (
    getTestR,
    getTestTimeR
) where

    import           Control.Monad.IO.Class
    import           Data.Time.Clock
    import           Domain
    import           Yesod

    getTestR :: Handler Html
    getTestR = defaultLayout $ do
        [whamlet| <p>The current time is: ^{getTime} |]

    getTestTimeR :: Handler Html
    getTestTimeR = simpleLayout getTime

    getTime :: Widget
    getTime = do
        now <- liftIO getCurrentTime
        [whamlet|
            <span hx-get="/test/time"
                    hx-trigger="load delay:30s" 
                    hx-swap="outerHTML"> #{show now} |]


            
