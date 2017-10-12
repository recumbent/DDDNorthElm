module Main exposing (..)

-- Import the things we might need

import Html exposing (Html, Attribute, h1, h2, div, text, input, ul, li)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json
import Time exposing (Time, second)
import Date

-- MODEL


-- type alias Item =
--     { id : Int
--     , name : String
--     , required : Bool
--     }


-- type alias ItemList =
--     List Item


type alias Model =
    { inputText : String
    , name : String
    , timeStamp : Time
--    , items : ItemList
    }


model : Model
model =
    { inputText = ""
    , name = ""
    , timeStamp = 0.0
--    , items = []
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = ChangeInput String
    | ChangeName
    | Tick Time    
--    | SelectItem


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( (case msg of
        ChangeInput input ->
            { model | inputText = input }

        ChangeName ->
            { model | name = model.inputText, inputText = "" }

        Tick newTime ->
            { model | timeStamp = newTime }

        -- SelectItem ->
        --     -- Step 1 change the model but do the same thing
        --     { model | items = { id = 0, name = model.inputText, required = True } :: model.items, inputText = "" }
      )
    , Cmd.none
    )



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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Simple elm application" ]
        , input
            [ placeholder "Name to greet"
            , onInput ChangeInput
            , onEnter ChangeName
            , value model.inputText
            ]        
        -- [ placeholder "Item to add"
        -- , onInput ChangeInput
        -- , onEnter SelectItem
        -- , value model.inputText
        -- ]
            []
        , h2 [] [ text ("Hello " ++ model.name ++ "! The time is " ++ (formatTime model.timeStamp)) ]
        -- , ItemListView model.items
--      --   , sortedItemListView model.items
        ]

formatTime : Time -> String
formatTime time =
    let
        date =
            Date.fromTime time
    in
        formatTimePart (Date.hour date) ++ ":" ++ formatTimePart (Date.minute date) ++ ":" ++ (formatTimePart (Date.second date))

formatTimePart : a -> String
formatTimePart timePart =
    toString timePart
        |> String.padLeft 2 '0'

-- sortedItemListView : List Item -> Html msg
-- sortedItemListView itemList =
--     List.sortBy .name itemList
--         |> itemListView


-- itemListView : List Item -> Html msg
-- itemListView itemList =
--     div []
--         [ ul []
--             (List.map itemView itemList)
--         ]


-- itemView : Item -> Html msg
-- itemView item =
--     li [] [ text item.name ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
