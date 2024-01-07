{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Location (
    Location(..),
    HasLocation(..),
    startGetLocation,
    getLocation
) where

    import           Control.Exception
    import           Control.Monad             (when)
    import           Data.Aeson
    import           Data.Text                 (Text)
    import           Network.HTTP.Client
    import           Network.HTTP.Types.Status
    import           System.Exit               (die)
    import           Yesod

    data Location = Location {
        lat :: Float,
        lng :: Float
    } deriving (Show, Read, Ord, Eq)

    instance FromJSON Location where
        parseJSON = withObject "Location" $ \obj -> do
                        status :: Text <- obj .: "status"
                        when (status /= "success") $
                            error $ "status is not success: " ++ show status
                        Location
                            <$> obj .: "lat"
                            <*> obj .: "lon"

    class HasLocation a where
        getLocationField :: a -> Location

    getLocation :: forall m .
                    (MonadHandler m
                    , HasLocation (HandlerSite m))
                    => m Location
    getLocation = getLocationField <$> getYesod

    startGetLocation :: IO Location
    startGetLocation = do
            r :: Either SomeException Location <- try go
            case r of
                Left (SomeException e) -> do
                    die $ "Getting location failed: " ++ show e
                Right loc -> pure loc
        where
            go :: IO Location
            go = do
                manager <- newManager defaultManagerSettings
                -- Note: https does NOT work here!  This must be http!
                req <- parseRequest "http://ip-api.com/json"
                resp <- httpLbs req manager
                if (statusIsSuccessful (responseStatus resp))
                then
                    case decode (responseBody resp) of
                        Just loc -> pure loc
                        Nothing ->
                            error $ "Failed to decode body: "
                                        ++ show (responseBody resp)
                else
                    error $ "Failed to get location, status code "
                                ++ show (statusCode (responseStatus resp))
                                ++ " ("
                                ++ show (statusMessage (responseStatus resp))
                                ++ ")"


