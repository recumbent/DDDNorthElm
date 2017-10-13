module Main exposing (..)

-- Import the things we might need

import Html exposing (Html, Attribute, h1, h2, div, text, input, ul, li, table, thead, th, tbody, tr, td)
import Html.Attributes exposing (placeholder, value, type_, checked)
import Html.Events exposing (onInput, on, keyCode, onCheck)
import Json.Decode as Json
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import RemoteData exposing (..)
import Http exposing (Error)


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
    , items : RemoteData Error ItemList
    }


model : Model
model =
    { inputText = ""
    , items = Loading
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( model
    , getItemList
    )



-- COMMANDS


serverUrl : String
serverUrl =
    "http://127.0.0.1:4000/items"


itemUrl : a -> String
itemUrl id =
    String.join "/" [ serverUrl, (toString id) ]


getItemList : Cmd Msg
getItemList =
    Http.get serverUrl decodeItems
        |> RemoteData.sendRequest
        |> Cmd.map ItemsResponse


decodeItems : Json.Decoder (List Item)
decodeItems =
    (Json.list itemDecoder)


itemDecoder : Json.Decoder Item
itemDecoder =
    Pipeline.decode Item
        |> Pipeline.required "id" Json.int
        |> Pipeline.required "name" Json.string
        |> Pipeline.required "required" Json.bool


setItemStateRequest : a -> Bool -> Http.Request Item
setItemStateRequest id state =
    Http.request
        { body = itemStateEncoder state |> Http.jsonBody
        , expect = Http.expectJson itemDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = itemUrl id
        , withCredentials = False
        }


itemStateEncoder : Bool -> Encode.Value
itemStateEncoder required =
    let
        attributes =
            [ ( "required", Encode.bool required ) ]
    in
        Encode.object attributes


setItemStateCmd : a -> Bool -> Cmd Msg
setItemStateCmd id state =
    setItemStateRequest id state
        |> RemoteData.sendRequest
        |> Cmd.map OnSetItemState


-- newItemCmd : { a | id : Int, name : String, required : Bool } -> Cmd Msg
-- newItemCmd item =
--     newItemRequest item
--         |> RemoteData.sendRequest
--         |> Cmd.map OnNewItem


-- newItemRequest : { a | id : Int, name : String, required : Bool } -> Http.Request Item
-- newItemRequest item =
--     let
--         itemBody =
--             itemEncoder item |> Http.jsonBody
--     in
--         Http.post serverUrl itemBody itemDecoder


-- itemEncoder : { a | id : Int, name : String, required : Bool } -> Encode.Value
-- itemEncoder item =
--     let
--         attributes =
--             [ ( "id", Encode.int item.id )
--             , ( "name", Encode.string item.name )
--             , ( "required", Encode.bool item.required )
--             ]
--     in
--         Encode.object attributes



-- UPDATE


type Msg
    = ChangeInput String
    | SelectItem
    | ToggleRequired Int Bool
    | ItemsResponse (RemoteData Error ItemList)
    | OnSetItemState (RemoteData Error Item)
--    | OnNewItem (RemoteData Error Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeInput input ->
            ( { model | inputText = input }, Cmd.none )

        SelectItem ->
            ( { model | items = (RemoteData.map (doSelectItem model.inputText) model.items), inputText = "" }, Cmd.none )
        
            -- case model.items of
            --     Success itemList ->
            --         let
            --             match =
            --                 List.filter (\i -> String.contains model.inputText i.name) itemList
            --                     |> List.head
            --         in
            --             case match of
            --                 Nothing ->
            --                     let
            --                         maxId =
            --                             Maybe.withDefault 0 (List.maximum (List.map (\i -> i.id) itemList))

            --                         newItem =
            --                             { id = maxId + 1, name = model.inputText, required = True }
            --                     in
            --                         ( { model | items = Success (newItem :: itemList), inputText = "" }
            --                         , newItemCmd newItem
            --                         )

            --                 Just item ->
            --                     ( { model | items = RemoteData.map (\items -> setItemRequiredState item.id True items) model.items }
            --                     , (setItemStateCmd item.id True)
            --                     )

            --     _ ->
            --         ( model
            --         , Cmd.none
            --         )

        ToggleRequired id state ->
            ( { model | items = RemoteData.map (\items -> setItemRequiredState id state items) model.items }
            , (setItemStateCmd id state)
            )

        ItemsResponse responseData ->
            ( { model | items = responseData }, Cmd.none )

        OnSetItemState responseData ->
            ( model, Cmd.none )

        -- OnNewItem responseData ->
        --     ( model, Cmd.none )


doSelectItem : String -> List Item -> List Item
doSelectItem inputText itemList =
    let
        match =
            List.filter (\i -> String.contains inputText i.name) itemList
                |> List.head
    in
        case match of
            Nothing ->
                let
                    maxId =
                        Maybe.withDefault 0 (List.maximum (List.map (\i -> i.id) itemList))
                in
                    ({ id = maxId + 1, name = inputText, required = True } :: itemList)

            Just item ->
                setItemRequiredState item.id True itemList


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
        , case model.items of
            NotAsked ->
                h2 [] [ text "Should never see this" ]

            Loading ->
                h2 [] [ text "Loading items..." ]

            Failure err ->
                h2 [] [ text ("HTTP Failure: " ++ (toString err)) ]

            Success items ->
                successView model.inputText items
        ]


successView : String -> List Item -> Html Msg
successView inputText items =
    div []
        [ input
            [ placeholder "Item to add"
            , onInput ChangeInput
            , onEnter SelectItem
            , value inputText
            ]
            []
        , filteredSortedItemListView inputText items
        ]


filteredSortedItemListView : String -> List Item -> Html Msg
filteredSortedItemListView filterText items =
    List.filter (\i -> String.contains filterText i.name) items
        |> List.sortBy .name
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
