{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Yesod.Htmx (
    pollArgs,
    delayToHour,
    getLocalTime,
    friendlyDate
) where

    import           Control.Monad.IO.Class
    import           Data.Fixed             (Pico)
    import           Data.Text              (Text)
    import qualified Data.Text              as Text
    import           Data.Time
    import           Domain
    import           Yesod

    -- | Create args to reload a DOM Node after a set delay.
    --
    pollArgs :: MonadHandler m =>
                Route (HandlerSite m)
                    -- ^ Route to fetch the updated HTML from.
                -> NominalDiffTime
                    -- ^ How long to wait before updating the HTML.
                -> m [ (Text, Text) ]
                    -- ^ The HTMX arguments, for *{ } interpolation
                    -- in Hamlet.
    pollArgs reloadUrl pollTime = do
        let secs :: Integer
            secs = ceiling pollTime
        urlRender <- getUrlRender
        pure $ [
            ("hx-get", urlRender reloadUrl),
            ("hx-trigger", Text.pack ("load delay:" ++ show secs ++ "s")),
            ("hx-swap", "outerHTML") ]

    delayToHour :: TimeOfDay -> Int -> NominalDiffTime
    delayToHour now h =
        let minutes :: Int
            minutes = 60

            hours :: Int
            hours = 60 * minutes

            s1 :: Int
            s1 = (h * hours)
                    - (todHour now * hours)
                    - (todMin now * minutes)
                    + 1

            s2 :: Pico
            s2 = (fromIntegral s1) + todSec now
        in
        secondsToNominalDiffTime s2

    getLocalTime :: MonadIO m => m LocalTime
    getLocalTime = liftIO $ do
        utcNow :: UTCTime <- getCurrentTime
        tzone  :: TimeZone <- getCurrentTimeZone
        pure $ utcToLocalTime tzone utcNow
 
    friendlyDate :: LocalTime -> Day -> Text
    friendlyDate now day = go
        where
            go :: Text
            go
                | today == day = "Today"
                | numDays == 1 = "Tomorrow"
                | numDays < 7  = Text.pack ("This " ++ show (dayOfWeek day))
                | otherwise    = Text.pack $
                                    formatTime
                                        defaultTimeLocale
                                        "%A, %e %B %0Y"
                                        day

            today :: Day
            today = localDay now

            numDays :: Integer
            numDays = diffDays day today
