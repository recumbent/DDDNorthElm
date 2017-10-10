module Main exposing (..)

-- Import the things we might need
import Html exposing (Html, h1, h2, div, text)

-- MODEL

model = 0


-- UPDATE

type Msg =
    NOOP

update msg model =
    case msg of
        NOOP -> model


-- VIEW

view model =
    div [] 
    [ h1 [] [ text "Simple elm application"]
    , h2 [] [ text "Hello DDD North" ]    
    ]


-- MAIN

main =
    Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }
