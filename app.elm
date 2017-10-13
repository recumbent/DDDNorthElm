module Main exposing (..)

-- Import the things we might need

import Html exposing (Html, Attribute, h1, h2, div, text, input, ul, li, table, thead, th, tbody, tr, td)
import Html.Attributes exposing (placeholder, value, type_, checked)
import Html.Events exposing (onInput, on, keyCode, onCheck)
import Json.Decode as Json


-- MODEL

type alias Item =
    { id : Int
    , name : String
    , required : Bool
    }


type alias ItemList =
    List Item


type alias Model =
    { inputText : String
    , items : ItemList
    }


model : Model
model =
    { inputText = ""
    , items = []
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = ChangeInput String
    | SelectItem
    | ToggleRequired Int Bool


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( (case msg of
        ChangeInput input ->
            { model | inputText = input }

        SelectItem ->
            let
                match =
                    List.filter (\i -> String.contains model.inputText i.name) model.items
                        |> List.head
            in
                case match of
                    Nothing ->
                        let
                            maxId =
                                Maybe.withDefault 0 (List.maximum (List.map (\i -> i.id) model.items))
                        in
                            { model | items = { id = maxId + 1, name = model.inputText, required = True } :: model.items, inputText = "" }

                    Just item ->
                        { model
                            | items = setItemRequiredState item.id True model.items
                            , inputText = ""
                        }

        ToggleRequired id state ->
            { model | items = setItemRequiredState id state model.items }
      )
    , Cmd.none
    )

setItemRequiredState : a -> b -> List { c | id : a, required : b } -> List { c | id : a, required : b }
setItemRequiredState id state itemList =
    List.map
        (\i ->
            if i.id == id then
                { i | required = state }
            else
                i
        )
        itemList



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



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Simple elm application" ]
        , input
            [ placeholder "Item to add"
            , onInput ChangeInput
            , onEnter SelectItem
            , value model.inputText
            ]
            []

        -- , sortedItemListView model.items
        , filteredSortedItemListView model.inputText model.items
        ]


filteredSortedItemListView : String -> List Item -> Html Msg
filteredSortedItemListView filterText items =
    List.filter (\i -> String.contains filterText i.name) items
        |> sortedItemListView


sortedItemListView : List Item -> Html Msg
sortedItemListView itemList =
    List.sortBy .name itemList
        |> itemListView

itemListView : List Item -> Html Msg
itemListView itemList =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "ID" ]
                , th [] [ text "Item Name" ]
                , th [] [ text "Required?" ]
                ]
            ]
        , tbody [] (List.map itemView itemList)
        ]


itemView : Item -> Html Msg
itemView item =
    tr []
        [ td [] [ text (toString item.id) ]
        , td [] [ text item.name ]
        , td [] [ input [ type_ "checkbox", (checked item.required), onCheck (ToggleRequired item.id) ] [] ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
