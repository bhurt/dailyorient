{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Domain (
    Domain(..),
    makeDomain,
    resourcesDomain,
    Widget,
    Handler,
    Route(..),
    simpleLayout
) where

    import           Data.Text           (Text)
    import           Server.Yesod.Routes
    import           Yesod

    data Domain = Domain

    mkYesodData domainName routes

    -- Note: this has to be *after* mkYesodData or it won't compile.
    instance Yesod Domain where

        makeSessionBackend _ = pure Nothing

        yesodMiddleware = id

        defaultLayout w = do
            p <- widgetToPageContent (setTitle "The Daily Orientation" <> w)
            msgs <- getMessages
            let description :: Text
                description =
                    case pageDescription p of
                        Nothing -> "The Daily Orientation"
                        Just x  -> x
            withUrlRenderer [hamlet|
                $newline never
                $doctype 5
                <html>
                    <head>
                        <meta charset="UTF-8">
                        <meta name="viewport"
                            content="width=device-width, initial-scale=1.0">
                        <meta name="author" content="Brian Hurt">
                        <title>#{pageTitle p}
                        <meta name="description" content="#{description}">
                        <script
                            src="https://unpkg.com/htmx.org@1.9.10"
                            integrity="sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
                            crossorigin="anonymous">
                        ^{pageHead p}

                    <body>
                        $forall (status, msg) <- msgs
                            <p class="message #{status}">#{msg}
                        ^{pageBody p}
            |]

    makeDomain :: IO Domain
    makeDomain = pure Domain
    

    simpleLayout :: Widget -> Handler Html
    simpleLayout w = do
        p <- widgetToPageContent w
        withUrlRenderer [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <meta charset="UTF-8">
                <body>
                    ^{pageBody p}
        |]


