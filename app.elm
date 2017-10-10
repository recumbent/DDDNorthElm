module Main exposing (..)

-- Import the things we might need
import Html exposing (Html, h1, h2, div, text, input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)

-- MODEL
type alias Model =
    { name : String        
    }

model : Model
model = 
     { name = "" }


-- UPDATE

type Msg =
    ChangeName String
    
update : Msg -> { b | name : a } -> { b | name : String }
update msg model =
    case msg of
        ChangeName newName -> { model | name = newName }


-- VIEW

view : { a | name : String } -> Html Msg
view model =
    div [] 
    [ h1 [] [ text "Simple elm application"]
    , input [ placeholder "Name to greet", onInput ChangeName ] []
    , h2 [] [ text ("Hello " ++ model.name ++ "!") ]    
    ]


-- MAIN

main : Program Never Model Msg
main =
    Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }
