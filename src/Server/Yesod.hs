{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}


-- mkYesodDispatch defines orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server.Yesod (
    yesodServer
) where

    import           Domain
    import           Server.Yesod.Handler.Cleaning
    import           Server.Yesod.Handler.Greeting
    import           Server.Yesod.Handler.Holiday
    import           Server.Yesod.Handler.Home
    import           Server.Yesod.Handler.Lfia
    import           Server.Yesod.Handler.TodayIs
    import           Server.Yesod.Handler.Weather
    import           Server.Yesod.Routes
    import           Yesod

    mkYesodDispatch domainName routes

    yesodServer :: Domain -> IO Application
    yesodServer = toWaiAppPlain 



