{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Weather (
    getWeatherR,
    weather
) where

    import qualified Data.IntMap.Strict as IntMap
    import           Data.Text          (Text)
    import           Data.Time
    import           Domain
    import           Server.Yesod.Htmx
    import           Weather
    import           Yesod

    weather :: Widget
    weather = do
        args <- pollArgs WeatherR 0
        wloc <- getWeatherLoc
        [whamlet|
            <div class="column">
                <div class="row label">
                    Weather for #{weatherCity wloc}, #{weatherState wloc}:
                <div class="row" *{args}>
                    Loading Forecast...
        |]

    getWeatherR :: Handler Html
    getWeatherR = simpleLayout $ do
            now <- getLocalTime
            wfoc <- getWeatherForecast now
            args <- pollArgs WeatherR
                        (diffLocalTime (weatherValidUntil wfoc) now)
            case take 3 (IntMap.toAscList (weatherForecast wfoc)) of
                [] ->
                    [whamlet|
                        <div class="row" *{args}>
                            Error fetching weather forecast (check the logs).
                    |]
                f : fs ->
                    [whamlet|
                        <div class="row" *{args}>
                            <div class="column">
                                ^{ mkt periodDetailedForecast f }
                                ^{ mapM_ (mkt periodShortForecast) fs}
                                <div class="topspace">
                    |]
        where
            mkt :: (WeatherPeriod -> Text) -> (Int, WeatherPeriod) -> Widget
            mkt fc (_, wp) = do
                let t :: Text
                    t = periodName wp <> ": " <> fc wp
                [whamlet|
                    <div class="row topspace">#{t}
                |]

