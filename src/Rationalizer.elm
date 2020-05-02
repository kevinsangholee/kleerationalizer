module Rationalizer exposing
    ( Ingred(..)
    , IngredListMessage(..)
    , IngredListState(..)
    )

import Array exposing (length, slice, toList)
import Browser
import Html exposing (Html, button, div, h1, input, li, ol, p, text, ul)
import Html.Attributes exposing (class, disabled, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (foldr, head, intersperse, length)


type Ingred
    = Ingred
        { ingredName : Maybe String
        , ingredQuantity : Float
        , ingredMeasurement : String
        }


type IngredListState
    = IngredListState
        { ingredList : List Ingred
        , newIngred : String
        , ingredValid : Bool
        }


type IngredListMessage
    = AddedIngred Ingred
    | FinishedIngred Ingred
    | UpdatedNewIngred String


main : Program () IngredListState IngredListMessage
main =
    Browser.element
        { init = ingredInit
        , update = ingredUpdate
        , view = ingredView
        , subscriptions = ingredSubscriptions
        }


ingredInit : () -> ( IngredListState, Cmd IngredListMessage )
ingredInit _ =
    let
        st =
            IngredListState { ingredList = [], newIngred = "", ingredValid = False }
    in
    ( st, Cmd.none )


ingredSubscriptions : IngredListState -> Sub IngredListMessage
ingredSubscriptions _ =
    Sub.none


ingredItem : Ingred -> Html IngredListMessage
ingredItem ingred =
    case ingred of
        Ingred ingredient ->
            li [] [ text (ingredToString (Ingred ingredient)), button [ onClick (FinishedIngred (Ingred ingredient)) ] [ text "-" ] ]



-- ENTRY OF INGREDIENTS


parseIngredient : String -> Ingred
parseIngredient toParse =
    let
        parts =
            Array.fromList (String.split " " toParse)

        name =
            if Array.length parts == 2 then
                Nothing

            else
                Just (foldr (++) "" (intersperse " " (toList (slice 2 (Array.length parts) parts))))
    in
    Ingred
        { ingredName = name
        , ingredQuantity = Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "" (Array.get 0 parts)))
        , ingredMeasurement = Maybe.withDefault "" (Array.get 1 parts)
        }


isValidIngred : String -> Bool
isValidIngred raw =
    let
        lst =
            String.split " " raw
    in
    if List.length lst < 2 then
        False

    else
        case head lst of
            Nothing ->
                False

            Just num ->
                case String.toFloat num of
                    Nothing ->
                        False

                    Just _ ->
                        True


ingredToString : Ingred -> String
ingredToString ing =
    case ing of
        Ingred ingred ->
            case ingred.ingredName of
                Nothing ->
                    String.fromFloat ingred.ingredQuantity ++ " " ++ ingred.ingredMeasurement

                Just name ->
                    String.fromFloat ingred.ingredQuantity ++ " " ++ ingred.ingredMeasurement ++ " " ++ name


ingredForm : String -> Bool -> Html IngredListMessage
ingredForm ingredText ingredValid =
    let
        changeIngred newString =
            UpdatedNewIngred newString

        displaySetting =
            if ingredValid then
                "none"

            else
                "block"
    in
    div []
        [ input [ value ingredText, onInput changeIngred ] []
        , button [ disabled (not ingredValid), onClick (AddedIngred (parseIngredient ingredText)) ] [ text "Add Ingredient" ]
        , p [ style "color" "red", style "display" displaySetting ] [ text "Ingredient not valid!" ]
        ]


ingredView : IngredListState -> Html IngredListMessage
ingredView (IngredListState { ingredList, newIngred, ingredValid }) =
    div [ class "container" ]
        [ h1 [] [ text "Rationalize Your Recipes!" ]
        , p [ class "instructions" ] [ text "Enter ingredients below of the form '2 tbsp sugar' or '3 eggs'" ]
        , ul [] (List.map ingredItem ingredList)
        , ingredForm newIngred ingredValid
        ]


scalingView : IngredListState


ingredUpdate : IngredListMessage -> IngredListState -> ( IngredListState, Cmd IngredListMessage )
ingredUpdate msg (IngredListState { ingredList, newIngred, ingredValid }) =
    case msg of
        AddedIngred newIngred_ ->
            let
                st =
                    IngredListState
                        { ingredList = newIngred_ :: ingredList
                        , newIngred = ""
                        , ingredValid = False
                        }
            in
            ( st, Cmd.none )

        FinishedIngred doneIngred ->
            let
                st =
                    IngredListState
                        { ingredList = List.filter (ingredsNotEqual doneIngred) ingredList
                        , newIngred = newIngred
                        , ingredValid = ingredValid
                        }
            in
            ( st, Cmd.none )

        UpdatedNewIngred raw ->
            ( IngredListState { ingredList = ingredList, newIngred = raw, ingredValid = isValidIngred raw }, Cmd.none )


ingredsNotEqual : Ingred -> Ingred -> Bool
ingredsNotEqual (Ingred t1) (Ingred t2) =
    t1.ingredName /= t2.ingredName || t1.ingredMeasurement /= t2.ingredMeasurement
