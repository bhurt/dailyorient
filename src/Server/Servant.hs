{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant (
    servantServer
) where

    import           Data.Proxy
    import           Domain
    import           Servant.API
    import           Servant.Server

    type API = "r" :> "v1" :> "ping" :> Get '[JSON] String

    server1 :: Server API
    server1 = pure "pong"

    servantServer :: Domain -> IO Application
    servantServer _ = pure $ serve (Proxy :: Proxy API) server1

