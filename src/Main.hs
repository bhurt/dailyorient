{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main(
    main
) where

    import           Data.Default
    import           Domain
    import           Network.HTTP.Types.Status                  (status403)
    import           Network.Wai
    import           Network.Wai.Handler.Warp
    import           Network.Wai.Middleware.Autohead
    import           Network.Wai.Middleware.Gzip
    import           Network.Wai.Middleware.HealthCheckEndpoint
    import           Network.Wai.Middleware.Local
    import           Network.Wai.Middleware.RequestLogger
    import           Network.Wai.Middleware.Timeout
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

            myGzipSettings :: GzipSettings
            myGzipSettings = def {
                                    gzipFiles = GzipCompress
                                }

            nonlocalResponse :: Response
            nonlocalResponse = responseLBS status403 [] "Local requests only"

            mware :: Middleware
            mware = 
                -- These are applied bottom to top
                autohead
                . healthCheck
                . gzip myGzipSettings
                . timeout 5
                . local nonlocalResponse
                . logStdoutDev

        putStrLn "Running server on port 3000."
        run 3000 (mware router)

