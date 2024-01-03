{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.TodayIs (
    getTodayIsDateR,
    getTodayIsTimeR,
    todayIs
) where

    import           Data.Time
    import           Domain
    import           Server.Yesod.Htmx
    import           Yesod

    todayIs :: LocalTime -> Widget
    todayIs now = do
        [whamlet|
            <div class="column" style="flex-grow: 1;">
                <div class="row label">Today is
                ^{todayIsDate now}
            <div class="column" style="padding-right: 1em;">
                <div class="row label">The time is
                ^{todayIsTime now}
        |]

    todayIsDate :: LocalTime -> Widget
    todayIsDate now = do
            let date :: String
                date = formatTime defaultTimeLocale "%A, %e %B %0Y" now

                delay :: NominalDiffTime
                delay = delayToHour (localTimeOfDay now) 24

            args <- pollArgs TodayIsDateR delay
            [whamlet|
                <div class="row" *{args}>#{date}
            |]
            
    todayIsTime :: LocalTime -> Widget
    todayIsTime now = do
        let date :: String
            date = formatTime defaultTimeLocale "%l:%M %p" now

            delay :: NominalDiffTime
            delay = secondsToNominalDiffTime (61 - todSec (localTimeOfDay now))

        args <- pollArgs TodayIsTimeR delay
        [whamlet|
            <div class="row" *{args}>#{date}
        |]
            


    getTodayIsDateR :: Handler Html
    getTodayIsDateR = simpleLayout $ do
                        now <- getLocalTime
                        todayIsDate now

    getTodayIsTimeR :: Handler Html
    getTodayIsTimeR = simpleLayout $ do
                        now <- getLocalTime
                        todayIsTime now

