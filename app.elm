module Main exposing (..)

-- Import the things we might need
import Html exposing (Html, h1, h2, div, text)

-- import Html exposing (Html, h1, h2, div, text, input)
-- import Html.Attributes exposing (placeholder)
-- import Html.Events exposing (onInput)

-- MODEL

-- type alias Model =
--     { name : String        
--     }

model = 0

-- model : Model
-- model = 
--     { name = "" }


-- UPDATE

type Msg =
    NOOP
--    ChangeName String
    
-- update : Msg -> { b | name : a } -> { b | name : String }
update msg model =
    case msg of
        NOOP -> model
--        ChangeName newName -> { model | name = newName }


-- VIEW

-- view : { a | name : String } -> Html Msg
view model =
    div [] 
    [ h1 [] [ text "Simple elm application"]
    , h2 [] [ text "Hello DDD North" ]  
    -- , input [ placeholder "Name to greet", onInput ChangeName ] []
    -- , h2 [] [ text ("Hello " ++ model.name ++ "!") ]    
    ]


-- MAIN

main : Program Never number Msg 
-- main : Program Never Model Msg
main =
    Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }
