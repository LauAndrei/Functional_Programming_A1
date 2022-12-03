module View.Day exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date)
import Util.Time exposing (formatDate)


{-| Don't modify
-}
type alias DailyData =
    { date : Date
    , highTemp : Maybe Float
    , lowTemp : Maybe Float
    , totalPrecipitaion : Float
    }


{-| Generates Html based on `DailyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}
view : DailyData -> Html msg
view data =
    let
        highTemp = Maybe.withDefault -1000 data.highTemp
        lowTemp = Maybe.withDefault -1000 data.lowTemp
    in
    
    div [class "day"] 
    [
        div [class "day-date"] [text <| formatDate data.date]
      , div [class "day-hightemp"] [text <| "High: " ++ if highTemp == -1000 then "unavailable" else String.fromFloat highTemp]
      , div [class "day-lowtemp"] [text <| "Low: " ++ if highTemp == -1000 then "unavailable" else String.fromFloat lowTemp]
      , div [class "day-precipitation"] [text <| "Total precipitation: " ++ String.fromFloat data.totalPrecipitaion]
    ]
    --Debug.todo "Implement View.Day.view"
