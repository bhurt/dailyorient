{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Weather (
    WeatherLoc(..),
    WeatherPeriod(..),
    WeatherForecast(..),
    WeatherData(..),
    HasWeatherDataField(..),
    startWeatherData,
    getWeatherLoc,
    getWeatherForecast
) where

    import           Control.Concurrent.MVar
    import           Control.DeepSeq
    import           Control.Exception
    import           Data.Aeson
    import           Data.Aeson.Types          (Parser)
    import qualified Data.ByteString.Lazy      as Lazy
    import           Data.IntMap.Strict        (IntMap)
    import qualified Data.IntMap.Strict        as IntMap
    import           Data.Text                 (Text)
    import           Data.Time.LocalTime
    import           Location
    import           Manager
    import           Network.HTTP.Client
    import           Network.HTTP.Client.TLS   (tlsManagerSettings)
    import           Network.HTTP.Types.Status
    import           System.Exit               (die)
    import           Yesod                     hiding (Key)



    data WeatherLoc = WeatherLoc {
        weatherCity :: Text,
        weatherState :: Text
    }

    instance NFData WeatherLoc where
        rnf wloc = rnf (weatherCity wloc)
                    `seq` rnf (weatherState wloc)
                    `seq` ()

    data WeatherPeriod = WeatherPeriod {
        periodName :: Text,
        periodShortForecast :: Text,
        periodDetailedForecast :: Text }

    instance NFData WeatherPeriod where
        rnf wper = rnf (periodName wper)
                    `seq` rnf (periodShortForecast wper)
                    `seq` rnf (periodDetailedForecast wper)
                    `seq` ()

    -- So I can hang a FromJSON instance off of it.
    newtype WeatherPeriodElem =
        WeatherPeriodElem { getWeatherPeriodElem :: (Int, WeatherPeriod) }

    instance FromJSON WeatherPeriodElem where
        parseJSON = withObject "WeatherPeriod" $ \obj -> do
            prd <- 
                WeatherPeriod
                    <$> obj .: "name"
                    <*> obj .: "shortForecast"
                    <*> obj .: "detailedForecast"
            num <- obj .: "number"
            return $ WeatherPeriodElem (num, prd)

    data WeatherForecast = WeatherForecast {
        weatherForecast :: IntMap WeatherPeriod,
        weatherValidUntil :: LocalTime
    }

    instance NFData WeatherForecast where
        rnf wfor = rnf (weatherForecast wfor)
                    `seq` rnf (weatherValidUntil wfor)
                    `seq` ()

    data WeatherData = WeatherData {
        weatherReq :: Request,
        weatherLoc :: WeatherLoc,
        weatherMVar :: MVar (Maybe WeatherForecast)
    }

    -- So I can hang a FromJSON instance off it.
    data WeatherRecord = WeatherRecord {
        wrReq :: Request,
        wrLoc :: WeatherLoc
    }

    instance FromJSON WeatherRecord where
        parseJSON =
            withObject "WeatherData" $ \obj -> do
                url :: String <- obj .: "forecast"
                case parseRequest url of
                    Nothing -> error $ "URL is could not be parsed: "
                                                ++ show url
                    Just req -> do
                        let parseRLoc :: Object -> Parser WeatherLoc
                            parseRLoc rloc = do
                                WeatherLoc
                                    <$> rloc .: "city"
                                    <*> rloc .: "state"
                        rlocVal :: Value <- obj .: "relativeLocation"
                        loc <- withObject "RelativeLocation" parseRLoc rlocVal
                        pure $ WeatherRecord {
                                    wrReq = req,
                                    wrLoc = loc }

    -- So I can hang a FromJSON instance off of it.
    newtype ForecastResponse = ForecastResponse {
                                    forecastResponse :: IntMap WeatherPeriod  }

    instance FromJSON ForecastResponse where
        parseJSON = withObject "ForecastResponse" $ \obj -> do
            elems :: [ WeatherPeriodElem ] <- obj .: "periods"
            let p2 :: [ (Int, WeatherPeriod) ]
                p2 = fmap getWeatherPeriodElem elems
                p3 :: IntMap WeatherPeriod
                p3 = IntMap.fromList p2
            pure $ ForecastResponse p3

    class HasWeatherDataField a where
        getWeatherDataField :: a -> WeatherData

    getWeatherLoc :: forall m .
                        (MonadHandler m
                        , HasWeatherDataField (HandlerSite m))
                        => m WeatherLoc
    getWeatherLoc = weatherLoc . getWeatherDataField <$> getYesod

    addWeatherHeaders :: Request -> Request
    addWeatherHeaders req =
        req {
            requestHeaders = [
                ("Accept", "application/ld+json"),
                ("User-Agent", "(dailyorient, bhurt42@gmail.com)")
            ]
        }

    startWeatherData :: Location -> IO WeatherData
    startWeatherData loc = do
            r :: Either SomeException WeatherData <- try go
            case r of
                Left (SomeException e) ->
                    die $ "Getting weather data failed: " ++ show e
                Right wd -> pure wd

        where
            go :: IO WeatherData
            go = do
                manager <- newManager tlsManagerSettings
                req1 <- parseRequest $ "https://api.weather.gov/points/"
                                        ++ show (lat loc)
                                        ++ ","
                                        ++ show (lng loc)
                let req = addWeatherHeaders req1
                resp <- httpLbs req manager
                if (statusIsSuccessful (responseStatus resp))
                then
                    case decode (responseBody resp) of
                        Just rec -> do
                            mvar <- newMVar Nothing
                            wlc <- evaluate . force $ wrLoc rec
                            pure $ WeatherData {
                                    weatherReq =
                                        addWeatherHeaders (wrReq rec),
                                    weatherLoc = wlc,
                                    weatherMVar = mvar }
                        Nothing -> do
                            putStrLn "Failed to decode Weather location"
                            putStrLn "Body:"
                            Lazy.putStr (responseBody resp)
                            putStrLn ""
                            error "Failed to decode body."
                else
                    error $ "Failed to get weather location, status code "
                                ++ show (statusCode (responseStatus resp))
                                ++ " ("
                                ++ show (statusMessage (responseStatus resp))
                                ++ ")"

    getWeatherForecast :: forall m .
                            (MonadHandler m
                            , HasWeatherDataField (HandlerSite m)
                            , HasManager (HandlerSite m))
                            => LocalTime
                            -> m WeatherForecast
    getWeatherForecast now = do
            hsite <- getYesod
            let wdata :: WeatherData
                wdata = getWeatherDataField hsite
            manager <- getManager
            liftIO $ modifyMVar (weatherMVar wdata) (go manager wdata)
        where
            go :: Manager
                    -> WeatherData
                    -> Maybe WeatherForecast
                    -> IO (Maybe WeatherForecast, WeatherForecast)
            go mgr wdata Nothing   = fetchNewForecast mgr wdata
            go mgr wdata (Just fc)
                | now < weatherValidUntil fc = pure (Just fc, fc)
                | otherwise                  = fetchNewForecast mgr wdata

            fetchNewForecast ::
                Manager
                -> WeatherData
                -> IO (Maybe WeatherForecast, WeatherForecast)
            fetchNewForecast mgr wd = do
                periods :: IntMap WeatherPeriod <- fetchPeriods mgr wd
                let lastHour :: LocalTime
                    lastHour =
                        now {
                            localTimeOfDay =
                                TimeOfDay {
                                    todHour =
                                        todHour
                                            (localTimeOfDay now),
                                    todMin = 0,
                                    todSec = 0 
                                }
                        }
                    
                    nextHour :: LocalTime
                    nextHour = addLocalTime 36300 lastHour

                    wf :: WeatherForecast
                    wf = WeatherForecast {
                                weatherForecast = periods,
                                weatherValidUntil = nextHour
                            }
                pure $ (Just wf, wf)

            -- If an error occurs here, we return a empty periods,
            -- and let the higher level code deal with it.
            fetchPeriods ::
                Manager
                -> WeatherData
                -> IO (IntMap WeatherPeriod)
            fetchPeriods mgr wd = do
                r :: Either SomeException (IntMap WeatherPeriod)
                    <- try $ do
                        resp <- httpLbs (weatherReq wd) mgr
                        if (statusIsSuccessful (responseStatus resp))
                        then
                            case decode (responseBody resp) of
                                Just rsp ->
                                    evaluate
                                        . force
                                        . forecastResponse
                                        $ rsp
                                Nothing  -> decodeFailed resp
                        else
                            reqFailed resp
                case r of
                    Left (SomeException e) -> do
                        putStrLn $ "Getting weather forecast threw exception: "
                                        ++ show e
                        pure $ IntMap.empty
                    Right x                -> pure x

            decodeFailed :: forall x . Response Lazy.ByteString -> IO (IntMap x)
            decodeFailed resp = do
                putStrLn "Failed to decode weather forecast."
                putStrLn "Body:"
                Lazy.putStr (responseBody resp)
                putStrLn ""
                pure $ IntMap.empty

            reqFailed :: forall x . Response Lazy.ByteString -> IO (IntMap x)
            reqFailed resp = do
                let stat = responseStatus resp
                putStrLn $ "Failed to get weather location, status code "
                            ++ show (statusCode stat)
                            ++ " ("
                            ++ show (statusMessage stat)
                            ++ ")"
                pure $ IntMap.empty
