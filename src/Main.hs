{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main(
    main
) where

    import           Domain
    import           Network.Wai
    import           Network.Wai.Handler.Warp
    import           Server.Servant
    import           Server.Yesod

    main :: IO ()
    main = do
        domain <- makeDomain
        ysd <- yesodServer domain
        svt <- servantServer domain
        let router :: Application
            router req resp = do
                case pathInfo req of
                    "r" : _ -> svt req resp
                    _       -> ysd req resp
        putStrLn "Running server on port 3000."
        run 3000 router

