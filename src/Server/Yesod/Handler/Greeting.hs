{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Greeting (
    getGreetingR,
    greeting
) where

    import           Data.Text         (Text)
    import           Data.Time
    import           Domain
    import           Server.Yesod.Htmx
    import           Yesod

    getGreetingR :: Handler Html
    getGreetingR = simpleLayout $ do
                        now <- getLocalTime
                        greeting now

    greeting :: LocalTime -> Widget
    greeting today = do
            let (greet, reload) = getGreeting
            args <- pollArgs GreetingR reload
            [whamlet|
                <div class="row greeting" *{args}>#{greet}
            |]
        where
            getGreeting :: (Text, NominalDiffTime)
            getGreeting = 
                let now :: TimeOfDay
                    now = localTimeOfDay today

                    waitTil :: Int -> NominalDiffTime
                    waitTil = delayToHour now
                in
                if | (todHour now < 6)  -> ("Good Night",     waitTil 6)
                   | (todHour now < 12) -> ("Good Morning",   waitTil 12)
                   | (todHour now < 17) -> ("Good Afternoon", waitTil 17)
                   | (todHour now < 22) -> ("Good Evening",   waitTil 22)
                   | otherwise          -> ("Good Night",     waitTil 30)
            
