{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Server.Yesod.Handler.Home (
    getHomeR
) where

    import           Data.Text                     (Text)
    import           Domain
    import qualified Server.Yesod.Handler.Greeting as Greeting
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
        toWidget [julius|

            function round_minute(dt) {
                let res = new Date(dt.getTime());
                res.setMilliseconds(0);
                res.setSeconds(0);
                return res;
            }

            function round_hour(dt) {
                let res = round_minute(dt);
                res.setMinutes(0);
                return res;
            }

            function round_day(dt) {
                let res = round_hour(dt);
                res.setHours(12);
                return res;
            }

            function round_month(dt) {
                let res = round_day(dt);
                res.setDate(1);
                return res;
            }

            function add_minutes(dt, n) {
                let res = round_minute(dt);
                res.setMinutes(res.getMinutes() + 1);
                return res;
            }

            function add_days(dt, n) {
                let res = round_day(dt);
                res.setDate(res.getDate() + n);
                return res;
            }

            function add_months(dt, n) {
                let res = round_month(dt);
                res.setMonth(res.getMonth() + n);
                return res;
            }

            function call_hourly(f) {
                let now = new Date();
                f(new Date(now.getTime()));
                let later = round_hour(now);
                later.setHours(later.getHours() + 1);
                let delayMillis = later - now + 10;
                setTimeout(call_hourly, delayMillis, f);
            }

            function call_daily(f) {
                let now = new Date();
                f(new Date(now.getTime()));
                let later = add_days(now, 1);
                later.setHours(0);
                let delayMillis = later - now + 10;
                setTimeout(call_daily, delayMillis, f);
            }

            const dayNames = [
                "Sunday", "Monday", "Tuesday", "Wednesday",
                "Thursday", "Friday", "Saturday" ];

            const monthNames = [
                "January", "February", "March", "April", "May",
                "June", "July", "August", "September",
                "October", "November", "December" ];

            function date_compare(dt1, dt2) {
                {
                    let y1 = dt1.getYear();
                    let y2 = dt2.getYear();
                    if (y1 < y2) {
                        return -1;
                    } else if (y1 > y2) {
                        return 1;
                    }
                }
                {
                    let m1 = dt1.getMonth();
                    let m2 = dt2.getMonth();
                    if (m1 < m2) {
                        return -1;
                    } else if (m1 > m2) {
                        return 1;
                    }
                }
                {
                    let d1 = dt1.getDate();
                    let d2 = dt2.getDate();
                    if (d1 < d2) {
                        return -1;
                    } else if (d1 > d2) {
                        return 1;
                    }
                }
                return 0;
            }

            function friendly_date(dt) {
                now = new Date();
                if (date_compare(now, dt) == 0) {
                    return "Today";
                }
                let tomorrow = add_days(now, 1);
                if (date_compare(tomorrow, dt) == 0) {
                    return "Tomorrow";
                }
                let week = add_days(now, 7);
                week.setHours(0);
                if (date_compare(dt, week) == -1) {
                    return "This " + dayNames[dt.getDay()];
                }
                var rval = 
                    dayNames[dt.getDay()]
                    + ", "
                    + monthNames[dt.getMonth()]
                    + " "
                    + dt.getDate().toString()
                    ;
                if (dt.getFullYear() != now.getFullYear()) {
                    rval = rval
                            + ", "
                            + dt.getFullYear().toString();
                }
                return rval;
            }

            function setTextById(nodeId, text) {
                let elem = document.getElementById(nodeId);
                if (elem == null) {
                    console.log("Could not find element with id \""
                                    + nodeId + "\"");
                    return;
                }
                elem.textContent = text;
            }
            |]
        [whamlet|
            <div class="column table">
                ^{Greeting.greeting now}
                ^{rowBlock (TodayIs.todayIs now)}
                ^{rowBlock nextLfia}
                ^{rowBlock nextCleaning}
                ^{rowBlock nextHoliday}
                ^{rowBlock weather}
        |]


    rowBlock :: Widget -> Widget
    rowBlock inner = [whamlet| <div class="row block"> ^{inner} |]

    nextLfia :: Widget
    nextLfia = do
        lfiaDiv :: Text <- newIdent
        [whamlet|
                <div class="column">
                    <div class="row label">Next LFIA is
                    <div class="row" id="#{lfiaDiv}">Eventually
        |]
        toWidget [julius|
            function calc_lfia(dt) {
                let res = round_month(dt);
                res.setDate(16);
                let dow = res.getDay();
                if (dow == 4) {
                    return res;
                } else if (dow > 4) {
                    res.setDate(27 - dow);
                } else {
                    res.setDate(20 - dow);
                }
                return res;
            }

            function update_lfia(now) {
                var lfia = calc_lfia(now);
                if (lfia.getDate() < now.getDate()) {
                    lfia = calc_lfia(add_months(now, 1));
                }
                setTextById(#{lfiaDiv}, friendly_date(lfia));
            }

            call_daily(update_lfia);
        |]

    nextCleaning :: Widget
    nextCleaning = do
        cleaningDiv :: Text <- newIdent
        [whamlet|
            <div class="column">
                <div class="row label">Cleaning lady comes
                <div class="row" id="#{cleaningDiv}">Someday
        |]
        toWidget [julius|
            const cleaning_start = round_day(new Date(2023, 6, 19));

            function update_cleaning(now) {
                let today = round_day(now);
                let numdays = Math.trunc(
                                (today - cleaning_start)
                                / (1000*60*60*24));
                var off = numdays % 14;
                if (off > 0) {
                    off = 14 - off;
                }
                let clean = add_days(today, off);
                setTextById(#{cleaningDiv}, friendly_date(clean));
            }
            call_daily(update_cleaning);
        |]

    nextHoliday :: Widget
    nextHoliday = mempty

    weather :: Widget
    weather = mempty
