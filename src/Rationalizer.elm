module Rationalizer exposing (..)

import Array exposing (append, length, push, slice, toList)
import Browser
import Html exposing (Html, button, div, h1, input, label, li, p, text, ul)
import Html.Attributes exposing (class, disabled, for, id, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (foldr, head, intersperse, length)


type Ingred
    = Ingred
        { ingredName : Maybe String
        , ingredQuantity : Float
        , ingredMeasurement : String
        }


type WebState
    = WebState
        { ingredList : List Ingred
        , newIngred : String
        , ingredValid : Bool
        }


type WebMessage
    = AddedIngred Ingred
    | FinishedIngred Ingred
    | UpdatedNewIngred String
    | MoveUpIngred Ingred
    | MoveDownIngred Ingred


main : Program () WebState WebMessage
main =
    Browser.element
        { init = ingredInit
        , update = webUpdate
        , view = mainView
        , subscriptions = ingredSubscriptions
        }


ingredInit : () -> ( WebState, Cmd WebMessage )
ingredInit _ =
    let
        st =
            WebState { ingredList = [], newIngred = "", ingredValid = False }
    in
    ( st, Cmd.none )


ingredSubscriptions : WebState -> Sub WebMessage
ingredSubscriptions _ =
    Sub.none


ingredItem : Ingred -> List Ingred -> Html WebMessage
ingredItem ingred lst =
    let
        ( moveUpValid, moveDownValid ) =
            ( findIngredIndex ingred lst 0 /= 0, findIngredIndex ingred lst 0 /= List.length lst - 1 )

        moveUpAttr =
            if moveUpValid then
                "visible"

            else
                "hidden"

        moveDownAttr =
            if moveDownValid then
                "visible"

            else
                "hidden"
    in
    li [ style "width" "100%", style "padding-bottom" "12px" ]
        [ p [ style "width" "40%", style "display" "inline-block", style "margin-left" "48px" ] [ text (ingredToString ingred) ]
        , button [ onClick (FinishedIngred ingred), style "margin-left" "24px", style "margin-right" "24px" ] [ text "-" ]
        , button [ onClick (MoveDownIngred ingred), style "margin-right" "24px", style "visibility" moveDownAttr ] [ text "v" ]
        , button [ onClick (MoveUpIngred ingred), style "visibility" moveUpAttr ] [ text "^" ]
        ]



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


ingredForm : String -> Bool -> Html WebMessage
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


mainView : WebState -> Html WebMessage
mainView state =
    div [ class "container" ]
        [ ingredView state
        , setScaleView state
        ]


ingredView : WebState -> Html WebMessage
ingredView (WebState { ingredList, newIngred, ingredValid }) =
    div [ class "ingredView" ]
        [ h1 [] [ text "Rationalize Your Recipes!" ]
        , p [ class "instructions" ] [ text "Enter ingredients below of the form '2 tbsp sugar' or '3 eggs'" ]
        , ingredForm newIngred ingredValid
        , div [ class "ingredListView", style "width" "100%" ]
            [ ul [ style "display" "block", style "margin-left" "auto", style "margin-right" "auto", style "width" "50%" ]
                (List.foldr (\x acc -> ingredItem x ingredList :: acc) [] ingredList)
            ]
        ]


setScaleView : WebState -> Html WebMessage
setScaleView (WebState _) =
    div [ class "setScaleView", style "display" "inline" ]
        [ p [] [ text "Set scale for rationalization: " ]
        , input [ type_ "radio", id "mult", name "scaleType", value "mult" ] []
        , label [ for "mult" ] [ text "*" ]
        , input [ type_ "radio", id "div", name "scaleType", value "div" ] []
        , label [ style "margin-left" "30px", for "div" ] [ text "/" ]
        , input [ style "margin-left" "50px", style "margin-right" "50px", style "width" "100px" ] []
        , button [] [ text "RATIONALIZE!" ]
        ]


webUpdate : WebMessage -> WebState -> ( WebState, Cmd WebMessage )
webUpdate msg (WebState { ingredList, newIngred, ingredValid }) =
    case msg of
        AddedIngred newIngred_ ->
            let
                st =
                    WebState
                        { ingredList = newIngred_ :: ingredList
                        , newIngred = ""
                        , ingredValid = False
                        }
            in
            ( st, Cmd.none )

        FinishedIngred doneIngred ->
            let
                st =
                    WebState
                        { ingredList = List.filter (ingredsNotEqual doneIngred) ingredList
                        , newIngred = newIngred
                        , ingredValid = ingredValid
                        }
            in
            ( st, Cmd.none )

        UpdatedNewIngred raw ->
            ( WebState { ingredList = ingredList, newIngred = raw, ingredValid = isValidIngred raw }, Cmd.none )

        MoveUpIngred ingredToMove ->
            ( WebState { ingredList = moveIngredUp ingredToMove ingredList, newIngred = newIngred, ingredValid = ingredValid }
            , Cmd.none
            )

        MoveDownIngred ingredToMove ->
            ( WebState { ingredList = moveIngredDown ingredToMove ingredList, newIngred = newIngred, ingredValid = ingredValid }
            , Cmd.none
            )


ingredsNotEqual : Ingred -> Ingred -> Bool
ingredsNotEqual (Ingred t1) (Ingred t2) =
    t1.ingredName /= t2.ingredName || t1.ingredMeasurement /= t2.ingredMeasurement || t1.ingredQuantity /= t2.ingredQuantity


findIngredIndex : Ingred -> List Ingred -> Int -> Int
findIngredIndex ingredToFind lst idx =
    case lst of
        [] ->
            -1

        x :: xs ->
            if ingredsNotEqual ingredToFind x then
                findIngredIndex ingredToFind xs (idx + 1)

            else
                idx


moveIngredDown : Ingred -> List Ingred -> List Ingred
moveIngredDown ingredToMove lst =
    let
        idx =
            findIngredIndex ingredToMove lst 0

        arr =
            Array.fromList lst
    in
    toList (append (push ingredToMove (append (slice 0 idx arr) (slice (idx + 1) (idx + 2) arr))) (slice (idx + 2) (Array.length arr) arr))


moveIngredUp : Ingred -> List Ingred -> List Ingred
moveIngredUp ingredToMove lst =
    let
        idx =
            findIngredIndex ingredToMove lst 0

        arr =
            Array.fromList lst
    in
    toList (append (append (push ingredToMove (slice 0 (idx - 1) arr)) (slice (idx - 1) idx arr)) (slice (idx + 1) (Array.length arr) arr))
