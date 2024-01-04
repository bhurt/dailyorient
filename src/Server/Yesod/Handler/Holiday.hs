{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Holiday (
    getHolidayR,
    nextHoliday
) where

    import           Data.Map.Strict   (Map)
    import qualified Data.Map.Strict   as Map
    import           Data.Text         (Text)
    import           Data.Time
    import           Domain
    import           Server.Yesod.Htmx
    import           Yesod

    holidays :: Map Day Text
    holidays = 
        let mk :: Text -> Year -> MonthOfYear -> DayOfMonth -> (Day, Text)
            mk name yr mn dy = (fromGregorian yr mn dy, name)
        in
        Map.fromAscList [
                mk "Martin Luther King Jr Day" 2024 January 15,
                mk "Valentine's Day" 2024 February 14,
                mk "Washington's Birthday" 2024 February 19,
                mk "Memorial Day" 2024 May 27,
                mk "Your Birthday" 2024 May 31,
                mk "Juneteenth" 2024 June 19,
                mk "Independence Day" 2024 July 4,
                mk "Labor Day" 2024 September 2,
                mk "Halloween" 2024 October 31,
                mk "Veterans Day" 2024 November 11,
                mk "Thanksgiving" 2024 November 28,
                mk "Christmas" 2024 December 25,
                mk "New Years Day" 2025 January 1 ]


    getHolidayR :: Handler Html
    getHolidayR = simpleLayout $ do
                    now <- getLocalTime
                    nextHoliday now

    nextHoliday :: LocalTime -> Widget
    nextHoliday now = do
        args <- pollArgs HolidayR (delayToHour (localTimeOfDay now) 24)
        let name :: Text
            date :: Day
            (date, name) =
                case Map.lookupGE (localDay now) holidays of
                    Just (d, n) -> (d, n)
                    Nothing     -> (localDay now,
                                    "Update your Holiday List!")
        [whamlet|
            <div class="column" style="flex-grow: 1;" *{args}>
                <div class="row label">The next holiday is
                <div class="row">#{name}
            <div class="column" style="padding-right: 0.5em;">
                <div class="row label">Which occurs on
                <div class="row">#{friendlyDate now date}
        |]

