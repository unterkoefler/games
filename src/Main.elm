module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


main =
        Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

update msg model =
        case msg of
                Increment ->
                        model + 1

                Decrement ->
                        model - 1

view model =
    layout
        []
        (column 
            []
        [ Input.button [] { onPress = Just Decrement, label = text "-" }
        , text (String.fromInt model)
        , Input.button [] { onPress = Just Increment, label = text "+" }
        ]
        )
