{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Greeting (
    getGreetingR,
    greeting
) where

    import           Data.Fixed        (Pico)
    import           Data.Text         (Text)
    import           Data.Time
    import           Domain
    import           Server.Yesod.Htmx
    import           Yesod

    getGreetingR :: Handler Html
    getGreetingR = simpleLayout greeting

    greeting :: Widget
    greeting = do
            (greet, reload) <- liftIO $ getGreeting
            args <- pollArgs GreetingR reload
            [whamlet|
                <div class="row greeting" *{args}>#{greet}
            |]
        where
            getGreeting :: IO (Text, NominalDiffTime)
            getGreeting = do
                utcNow :: UTCTime <- getCurrentTime
                tzone :: TimeZone <- getCurrentTimeZone
                let today :: LocalTime
                    today = utcToLocalTime tzone utcNow

                    now :: TimeOfDay
                    now = localTimeOfDay today

                    minutes :: Int
                    minutes = 60

                    hours :: Int
                    hours = 60 * minutes

                    waitTil :: Int -> NominalDiffTime
                    waitTil h = 
                        let s1 :: Int
                            s1 = (h * hours)
                                    - (todHour now * hours)
                                    - (todMin now * minutes)
                                    + 1

                            s2 :: Pico
                            s2 = (fromIntegral s1) + todSec now
                        in
                        secondsToNominalDiffTime s2

                pure $
                    if | (todHour now < 6)  -> ("Good Night",     waitTil 6)
                       | (todHour now < 12) -> ("Good Morning",   waitTil 12)
                       | (todHour now < 17) -> ("Good Afternoon", waitTil 17)
                       | (todHour now < 22) -> ("Good Evening",   waitTil 22)
                       | otherwise          -> ("Good Night",     waitTil 30)
            
