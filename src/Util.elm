module Util exposing (groupBy, maximumBy, maybeToList, minimumBy, zipFilter)

{-| Module containing utility functions
-}
import String exposing (toList)
import Json.Decode exposing (bool)
import Html exposing (a)
import Debug exposing (toString)

{-| Description for minimumBy

    minimumBy .x [ { x = 1, y = 2 } ] --> Just {x = 1, y = 2}

    minimumBy .x [] --> Nothing

    minimumBy (modBy 10) [ 16, 23, 14, 5 ] --> Just 23

-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy func list =
    let minBy x (y, fy) = 
            let fx = func x 
            in 
                if fx < fy then 
                    (x, fx) 
                else 
                    (y, fy)
    in 
        case list of
            [l] -> Just l
            l::ls -> Just <| Tuple.first <| List.foldl minBy (l, func l) ls
            _ -> Nothing



{-| Description for maximumBy

    maximumBy .x [ { x = 1, y = 2 } ] --> Just {x = 1, y = 2}

    maximumBy .x [] --> Nothing

    maximumBy (modBy 10) [ 16, 23, 14, 5 ] --> Just 16

-}
maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy func list =
    let maxBy x (y, fy) =
            let fx = func x
            in 
                if fx > fy then
                    (x, fx)
                else 
                    (y, fy)
    in
        case list of
            [l] -> Just l
            l::ls -> Just <| Tuple.first <| List.foldl maxBy (l, func l) ls
            _ -> Nothing
    


{-| Group a list

    groupBy .x [ { x = 1 } ] --> [(1, [{x = 1}])]

    groupBy (modBy 10) [ 11, 12, 21, 22 ] --> [(1, [11, 21]), (2, [12, 22])]

    groupBy identity [] --> []

-}
groupBy : (a -> b) -> List a -> List ( b, List a )
groupBy func list =
    let 
        unique lst =
            List.foldl (\a uniques -> 
                        if List.member a uniques then
                            uniques
                        else
                            uniques ++ [a]    
                    )
                    []
                    lst
        keys = list 
                |> List.map func
                |> unique

        group kl lst = 
            case kl of
                [] -> []
                k::ks -> (k, List.filter (\x -> func x == k) lst) :: group ks lst
    in
    group keys list



{-| Transforms a Maybe into a List with one element for Just, and an empty list for Nothing

    maybeToList (Just 1) --> [1]

    maybeToList Nothing --> []

-}
maybeToList : Maybe a -> List a
maybeToList number =
    case number of
        Just x -> [x]
        Nothing -> []


{-| Filters a list based on a list of bools

    zipFilter [ True, True ] [ 1, 2 ] --> [1, 2]

    zipFilter [ False, False ] [ 1, 2 ] --> []

    zipFilter [ True, False, True, False ] [ 1, 2, 3, 4 ] --> [1, 3]

-}
zipFilter : List Bool -> List a -> List a
zipFilter boolList unfilteredList =
    case (boolList, unfilteredList) of
        ([b], [l]) -> if (b == True) then
                        [l]
                    else
                        []

        (b::bs, l::ls) -> if (b == True) then
                            l :: zipFilter bs ls
                        else
                            zipFilter bs ls

        (_, _) -> unfilteredList

