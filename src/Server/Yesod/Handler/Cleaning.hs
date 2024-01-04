{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Cleaning (
    getCleaningR,
    nextCleaning
) where

    import           Data.Text         (Text)
    import           Data.Time
    import           Domain
    import           Server.Yesod.Htmx
    import           Yesod

    baseDay :: Day
    baseDay = fromGregorian 2024 January 3

    nextCleaning :: LocalTime -> Widget
    nextCleaning now = 
        [whamlet|
            <div class="column">
                <div class="row label">Cleaning lady comes
                ^{cleaningDate now}
        |]

    cleaningDate :: LocalTime -> Widget
    cleaningDate now = do
            args <- pollArgs CleaningR (delayToHour (localTimeOfDay now) 24)
            [whamlet|
                <div class="row" *{args} >#{txt}
            |]
        where
            txt :: Text
            txt = friendlyDate now nextDate

            nextDate :: Day
            nextDate = addDays numDays (localDay now)

            numDays :: Integer
            numDays = 14 - (diffDays (localDay now) baseDay `mod`14)

    getCleaningR :: Handler Html
    getCleaningR = simpleLayout $ do
                        now <- getLocalTime
                        cleaningDate now
