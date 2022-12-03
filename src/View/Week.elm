module View.Week exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date, formatDate)
import View.Day exposing (DailyData)


type alias WeeklyData =
    { dailyData : List DailyData
    }


{-| Generates Html based on `WeeklyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}
view : WeeklyData -> Html msg
view data =
    let
      firstDay = data.dailyData |> List.take 1
      lastDay = data.dailyData |> List.reverse |> List.take 1

      renderDailyData first last dataList = 
        case dataList of
            [] -> div [] []
            d::ds ->
              if [d] == first then
                div[class "day"]
                [
                  div [class "day-date"] [ h2[][ text <| formatDate d.date]]
                  , div [class "day-hightemp"][text <| "High: " ++ String.fromFloat (Maybe.withDefault 0 d.highTemp)]
                  , div [class "day-lowtemp"] [text <| "Low: " ++ String.fromFloat (Maybe.withDefault 0 d.lowTemp)]
                  , div [class "day-precipitation"] [text <| "Total precipitation: " ++ String.fromFloat d.totalPrecipitaion]
                ]
              else if [d] == last then
                div [class "day"][
                  div [class "day-date"] [ h2[][ text <| formatDate d.date]]
                  , div [class "day-hightemp"] [text <| "High: " ++ String.fromFloat (Maybe.withDefault 0 d.highTemp)]
                  , div [class "day-lowtemp"] [text <| "Low: " ++ String.fromFloat (Maybe.withDefault 0 d.lowTemp)]
                  , div [class "day-precipitation"] [text <| "Total precipitation: " ++ String.fromFloat d.totalPrecipitaion]
                ]
              else 
                View.Day.view d

    in
      div [class "week"] [renderDailyData firstDay lastDay data.dailyData]
