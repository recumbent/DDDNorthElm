module Main exposing (..)

-- Import the things we might need

import Html exposing (Html, Attribute, h1, h2, div, text, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json
-- import Time exposing (Time, second)
-- import Date


-- MODEL


type alias Model =
    { inputText : String
    , name : String
    -- , timeStamp : Time
    }


model : Model
model =
    { inputText = ""
    , name = ""
    -- , timeStamp = 0.0
    }



-- INIT


-- init : ( Model, Cmd Msg )
-- init =
--     ( model, Cmd.none )



-- UPDATE


type Msg
    = ChangeInput String
    | ChangeName
    -- | Tick Time


update : Msg -> { b | name : a, inputText : a } -> { b | inputText : String, name : a }
-- update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
--    ( (
    case msg of
        ChangeInput input ->
            { model | inputText = input }

        ChangeName ->
            { model | name = model.inputText, inputText = "" }

    --     Tick newTime ->
    --         { model | timeStamp = newTime }
    --   )
    -- , Cmd.none
    -- )



-- Borrowed from https://github.com/evancz/elm-todomvc/blob/master/Todo.elm


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)



-- SUBSCRIPTIONS


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     Time.every second Tick



-- VIEW


-- view : Model -> Html Msg
view : { a | inputText : String, name : String } -> Html Msg
view model =
    div []
        [ h1 [] [ text "Simple elm application" ]
        , input
            [ placeholder "Name to greet"
            , onInput ChangeInput
            , onEnter ChangeName
            , value model.inputText
            ]
            []
        , h2 [] [ text ("Hello " ++ model.name ++ "!") ]
--        , h2 [] [ text ("Hello " ++ model.name ++ "! The time is " ++ (formatTime model.timeStamp)) ]
        ]


-- formatTime time =
--     let
--         date =
--             Date.fromTime time
--     in
--         formatTimePart (Date.hour date) ++ ":" ++ formatTimePart (Date.minute date) ++ ":" ++ (formatTimePart (Date.second date))


-- formatTimePart timePart =
--     toString timePart
--         |> String.padLeft 2 '0'



-- MAIN


main : Program Never Model Msg
main =
    -- Html.program
    --     { init = init
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
--        , subscriptions = subscriptions
        }
