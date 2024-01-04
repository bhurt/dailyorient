{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Lfia (
    getLfiaR,
    lfia
) where

    import           Data.Text         (Text)
    import           Data.Time
    import           Domain
    import           Server.Yesod.Htmx
    import           Yesod

    lfia :: LocalTime -> Widget
    lfia now =
        [whamlet|
                <div class="column">
                    <div class="row label">Next LFIA is
                    ^{lfiaText now}
        |]


    lfiaText :: LocalTime -> Widget
    lfiaText now = do
            args <- pollArgs LfiaR (delayToHour (localTimeOfDay now) 24)
            [whamlet| <div class="row" *{args}>#{txt} |]
        where
            txt :: Text
            txt = friendlyDate now nextMeeting

            nextMeeting :: Day
            nextMeeting =
                let year :: Year
                    month :: MonthOfYear
                    (year, month, _) = toGregorian (localDay now)

                    t1 :: Day
                    t1 = lfiaForMonth year month
                in
                if (t1 > (localDay now))
                then t1
                else
                    let year' :: Year
                        month' :: MonthOfYear
                        (year', month') = nextMonth year month
                    in
                    lfiaForMonth year' month'

            lfiaForMonth :: Year -> MonthOfYear -> Day
            lfiaForMonth year month =
                let mid :: Day
                    mid = fromGregorian year month 15

                    plusDays :: Integer
                    plusDays =
                        case dayOfWeek mid of
                            Monday    -> 3
                            Tuesday   -> 2
                            Wednesday -> 1
                            Thursday  -> 7
                            Friday    -> 6
                            Saturday  -> 5
                            Sunday    -> 4
                in
                addDays plusDays mid

            nextMonth :: Year -> MonthOfYear -> (Year, MonthOfYear)
            nextMonth year month
                | month == December = (year + 1, January)
                | otherwise         = (year,     month + 1)

    getLfiaR :: Handler Html
    getLfiaR = simpleLayout $ do
        now <- getLocalTime
        lfiaText now

