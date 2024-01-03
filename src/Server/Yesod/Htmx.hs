{-# LANGUAGE OverloadedStrings #-}

module Server.Yesod.Htmx (
    pollArgs
) where

    import           Data.Text (Text)
    import qualified Data.Text as Text
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

