{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Home (
    getHomeR
) where

    import           Domain
    import qualified Server.Yesod.Handler.Cleaning as Cleaning
    import qualified Server.Yesod.Handler.Greeting as Greeting
    import qualified Server.Yesod.Handler.Lfia     as Lfia
    import qualified Server.Yesod.Handler.TodayIs  as TodayIs
    import           Server.Yesod.Htmx
    import           Yesod

    getHomeR :: Handler Html
    getHomeR = defaultLayout $ do
        now <- getLocalTime
        toWidget
            [lucius|
                .column {
                    display: flex;
                    flex-direction: column;
                }
                .row {
                    display: flex;
                    flex-direction: row;
                }
                html body .fullwidth {
                    width: 100%;
                }
                .block {
                    border: 1px solid #A0A0A0;
                    border-radius: 10px;
                    padding: 0.25em;
                    font-size: 1.5em;
                }
                .greeting {
                    justify-content: center;
                    padding: 0.25em;
                    font-size: 2em;
                }
                .table>:nth-child(4n+2) {
                    background: #F0F0FF;
                }
                .table>:nth-child(4n) {
                    background: #F0FFF0;
                }
                .label {
                    color: #404040;
                    font-size: 0.6em;
                }
            |]
        [whamlet|
            <div class="column table">
                ^{Greeting.greeting now}
                ^{rowBlock (TodayIs.todayIs now)}
                ^{rowBlock (Lfia.lfia now)}
                ^{rowBlock (Cleaning.nextCleaning now)}
                ^{rowBlock nextHoliday}
                ^{rowBlock weather}
        |]


    rowBlock :: Widget -> Widget
    rowBlock inner = [whamlet| <div class="row block"> ^{inner} |]

    nextHoliday :: Widget
    nextHoliday = mempty

    weather :: Widget
    weather = mempty
