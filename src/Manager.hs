{-# LANGUAGE FlexibleContexts #-}

module Manager (
    ManagerField,
    HasManager(..),
    startManager,
    getManager
) where

    import           Network.HTTP.Client
    import           Network.HTTP.Client.TLS (tlsManagerSettings)
    import           Yesod

    newtype ManagerField = ManagerField { getTheManager :: Manager }

    class HasManager a where
        getManagerField :: a -> ManagerField

    getManager :: (MonadHandler m, HasManager (HandlerSite m)) => m Manager
    getManager = getTheManager . getManagerField <$> getYesod

    startManager :: IO ManagerField
    startManager = ManagerField <$> newManager tlsManagerSettings

