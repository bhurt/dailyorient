{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Server.Yesod.Routes (
    domainName,
    routes
) where

    import           Yesod
    import           Yesod.Routes.TH.Types

    domainName :: String
    domainName = "Domain"

    routes :: [ ResourceTree String ]
    routes = [parseRoutes|
                /               HomeR           GET
                /cleaning       CleaningR       GET
                /greeting       GreetingR       GET
                /holiday        HolidayR        GET
                /lfia           LfiaR           GET
                /todayis/date   TodayIsDateR    GET
                /todayis/time   TodayIsTimeR    GET
                /weather        WeatherR        GET
             |]

