module Main exposing (..)

-- Import the things we might need
import Html exposing (Html, h1, h2, div, text, input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)

-- import Html exposing (Html, Attribute, h1, h2, div, text, input)
-- import Html.Attributes exposing (placeholder, value)
-- import Html.Events exposing (onInput, on, keyCode)
-- import Json.Decode as Json


-- MODEL


type alias Model =
    { name : String 
    -- inputText : String
    -- , name : String
    }


model : Model
model =
    -- { inputText = ""
    -- ,
    { name = ""
    }



-- UPDATE

type Msg =
    ChangeName String
    -- = ChangeInput String
    -- | ChangeName



update : Msg -> { b | name : a } -> { b | name : String }
-- update : Msg -> { b | name : a, inputText : a } -> { b | inputText : String, name : a }
update msg model =
    case msg of
        ChangeName newName -> { model | name = newName }
        -- ChangeInput input ->
        --     { model | inputText = input }

        -- ChangeName ->
        --     { model | name = model.inputText, inputText = "" }



-- Borrowed from https://github.com/evancz/elm-todomvc/blob/master/Todo.elm


-- onEnter : Msg -> Attribute Msg
-- onEnter msg =
--     let
--         isEnter code =
--             if code == 13 then
--                 Json.succeed msg
--             else
--                 Json.fail "not ENTER"
--     in
--         on "keydown" (Json.andThen isEnter keyCode)



-- VIEW


view : { a | name : String } -> Html Msg
-- view : { a | inputText : String, name : String } -> Html Msg
view model =
    div []
        [ h1 [] [ text "Simple elm application" ]
        , input
            [ placeholder "Name to greet", onInput ChangeName ] []
            -- [ placeholder "Name to greet"
            -- , onInput ChangeInput
            -- , onEnter ChangeName
            -- , value model.inputText
            -- ]
            -- []
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
