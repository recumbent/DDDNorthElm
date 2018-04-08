module Main exposing (..)

-- Import the things we might need

import Html exposing (Html, Attribute, h1, h2, div, text, input, ul, li, table, thead, th, tbody, tr, td, p, label, fieldset, button, hr, a)
import Html.Attributes exposing (placeholder, value, type_, checked, style)
import Html.Events exposing (onInput, on, keyCode, onCheck, onClick)
import Html.Keyed as Keyed
import Json.Decode as Json
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import RemoteData exposing (..)
import Http exposing (Error)
import Json.Decode exposing (Decoder, decodeValue, succeed, string, list)
import Aisle exposing (..)
import FontAwesome exposing (useSvg, iconWithOptions, edit)



-- MODEL


type Views
    = ItemList
    | ShoppingList
    | Checkout


type alias Item =
    { id : Int
    , name : String
    , required : Bool
    , purchased : Bool
    , aisle : Maybe Aisle
    }


type alias ItemList =
    List Item


type alias Model =
    { currentView : Views
    , inputText : String
    , items : RemoteData Error ItemList
    , lastAisle: Maybe Aisle
    }


model : Model
model =
    { currentView = ItemList
    , inputText = ""
    , items = Loading
    , lastAisle = Nothing
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
    -- "http://127.0.0.1:55944/items"
    -- "http://127.0.0.1:4000/items"
    -- "http://localhost:55943/items"
    "http://localhost:52417/items"


itemUrl : a -> String
itemUrl id =
    String.join "/" [ serverUrl, (toString id) ]


checkoutUrl : String
checkoutUrl =
    String.join "/" [ serverUrl, "checkout" ]


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
        |> Pipeline.required "Id" Json.int
        |> Pipeline.required "Name" Json.string
        |> Pipeline.required "Required" Json.bool
        |> Pipeline.required "Purchased" Json.bool
        |> Pipeline.optional "Aisle" (Json.map Just aisleDecoder) Nothing


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
            [ ( "Required", Encode.bool required )
            , ( "Purchased", Encode.bool False )
            ]
    in
        Encode.object attributes


itemPurchasedStateEncoder : Bool -> Encode.Value
itemPurchasedStateEncoder purchased =
    let
        attributes =
            [ ( "Purchased", Encode.bool purchased )
            ]
    in
        Encode.object attributes


itemAisleEncoder : Maybe Aisle -> Encode.Value
itemAisleEncoder aisle =
    let
        attributes =
            [ ( "Aisle", encodeAisle aisle )
            ]
    in
        Encode.object attributes


setItemStateCmd : a -> Bool -> Cmd Msg
setItemStateCmd id state =
    setItemStateRequest id state
        |> RemoteData.sendRequest
        |> Cmd.map OnSetItemState


setItemPurchasedStateRequest : a -> Bool -> Http.Request Item
setItemPurchasedStateRequest id state =
    Http.request
        { body = itemPurchasedStateEncoder state |> Http.jsonBody
        , expect = Http.expectJson itemDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = itemUrl id
        , withCredentials = False
        }


setItemPurchasedStateCmd : a -> Bool -> Cmd Msg
setItemPurchasedStateCmd id state =
    setItemPurchasedStateRequest id state
        |> RemoteData.sendRequest
        |> Cmd.map OnSetItemState


setItemAisleRequest : a -> Maybe Aisle -> Http.Request Item
setItemAisleRequest id aisle =
    Http.request
        { body = itemAisleEncoder aisle |> Http.jsonBody
        , expect = Http.expectJson itemDecoder
        , headers = []
        , method = "PATCH"
        , timeout = Nothing
        , url = itemUrl id
        , withCredentials = False
        }


setItemAisleCmd : a -> Maybe Aisle -> Cmd Msg
setItemAisleCmd id aisle =
    setItemAisleRequest id aisle
        |> RemoteData.sendRequest
        |> Cmd.map OnSetAisle


newItemCmd : Item -> Cmd Msg
newItemCmd item =
    newItemRequest item
        |> RemoteData.sendRequest
        |> Cmd.map OnNewItem


newItemRequest : Item -> Http.Request Item
newItemRequest item =
    let
        itemBody =
            itemEncoder item |> Http.jsonBody
    in
        Http.post serverUrl itemBody itemDecoder


encodeAisle : Maybe Aisle -> Encode.Value
encodeAisle aisle =
    case aisle of
        Nothing ->
            Encode.null

        Just a ->
            case a of
                None ->
                    Encode.int 0

                Number n ->
                    Encode.int n


itemEncoder : Item -> Encode.Value
itemEncoder item =
    let
        attributes =
            [ ( "Id", Encode.int item.id )
            , ( "Name", Encode.string item.name )
            , ( "Required", Encode.bool item.required )
            , ( "Purchased", Encode.bool item.purchased )
            , ( "Aisle", encodeAisle item.aisle )
            ]
    in
        Encode.object attributes


checkoutCmd : Cmd Msg
checkoutCmd =
    checkoutRequest
        |> RemoteData.sendRequest
        |> Cmd.map OnCheckout


checkoutRequest : Http.Request (List Item)
checkoutRequest =
    Http.post checkoutUrl Http.emptyBody decodeItems



-- UPDATE


type Msg
    = ChangeInput String
    | SelectItem
    | ToggleRequired Int Bool
    | ItemsResponse (RemoteData Error ItemList)
    | OnSetItemState (RemoteData Error Item)
    | OnNewItem (RemoteData Error Item)
    | SelectView Views
    | TogglePurchased Int Bool
    | IncAisle Int
    | DecAisle Int
    | DoCheckout
    | OnCheckout (RemoteData Error ItemList)
    | OnSetAisle (RemoteData Error Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeInput input ->
            ( { model | inputText = input }, Cmd.none )

        SelectItem ->
            case model.items of
                Success itemList ->
                    let
                        match =
                            List.filter (\i -> String.contains model.inputText i.name) itemList
                                |> List.head
                    in
                        case match of
                            Nothing ->
                                let
                                    maxId =
                                        Maybe.withDefault 0 (List.maximum (List.map (\i -> i.id) itemList))

                                    newItem : Item
                                    newItem =
                                        { id = maxId + 1, name = model.inputText, required = True, purchased = False, aisle = Nothing }
                                in
                                    ( { model | items = Success (newItem :: itemList), inputText = "" }
                                    , newItemCmd newItem
                                    )

                            Just item ->
                                ( { model | items = RemoteData.map (\items -> setItemRequiredState item.id True items) model.items, inputText = "" }
                                , (setItemStateCmd item.id True)
                                )

                _ ->
                    ( model
                    , Cmd.none
                    )

        ToggleRequired id state ->
            ( { model | items = RemoteData.map (setItemRequiredState id state) model.items }
            , (setItemStateCmd id state)
            )

        ItemsResponse responseData ->
            ( { model | items = responseData }, Cmd.none )

        OnSetItemState responseData ->
            ( model, Cmd.none )

        OnNewItem responseData ->
            ( model, Cmd.none )

        SelectView newView ->
            ( { model | currentView = newView }, Cmd.none )

        TogglePurchased id state ->
            let
                hacked =
                    RemoteData.toMaybe model.items
                        |> Maybe.map (List.partition (\i -> i.id == id))
                        |> Maybe.andThen (processTuple (\i -> { i | purchased = state}))
            in
                case hacked of 
                    Nothing ->
                        ( model, Cmd.none )
                    Just ( modified, list ) ->
                        ( { model | items = RemoteData.Success (modified :: list), lastAisle = modified.aisle } 
                        , (setItemPurchasedStateCmd id state)
                        )

        IncAisle id ->
            let
                hacked =
                    RemoteData.toMaybe model.items
                        |> Maybe.map (List.partition (\i -> i.id == id))
                        |> Maybe.andThen (processTuple (incrementItemAisle model.lastAisle))
            in
                case hacked of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ( modified, list ) ->
                        ( { model | items = RemoteData.Success (modified :: list) }
                        , (setItemAisleCmd id modified.aisle)
                        )

        DecAisle id ->
            let
                hacked =
                    RemoteData.toMaybe model.items
                        |> Maybe.map (List.partition (\i -> i.id == id))
                        |> Maybe.andThen (processTuple (decrementItemAisle model.lastAisle))
            in
                case hacked of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ( modified, list ) ->
                        ( { model | items = RemoteData.Success (modified :: list) }
                        , (setItemAisleCmd id modified.aisle)
                        )

        DoCheckout ->
            ( model
            , checkoutCmd
            )

        OnCheckout responseData ->
            ( { model | items = responseData, currentView = ItemList }
            , Cmd.none
            )

        OnSetAisle responseData ->
            ( model, Cmd.none )

getLogMessages : RemoteData Error a -> String
getLogMessages responseData =
    case responseData of
        Failure e ->
            case e of
                Http.BadPayload s r ->
                    s
                _ -> "Not a bad payload?"
        _ -> "Not a failure?" 

processTuple : (a -> b) -> ( List a, c ) -> Maybe ( b, c )
processTuple modifier tp =
    Tuple.first tp
        |> List.head
        |> Maybe.map modifier
        |> Maybe.map (\m -> ( m, (Tuple.second tp) ))

incrementItemAisle : Maybe Aisle -> { a | aisle : Maybe Aisle } -> { a | aisle : Maybe Aisle }
incrementItemAisle lastAisle item =
    case item.aisle of
        Nothing ->
            case lastAisle of
                Nothing -> { item | aisle = Just None }
                Just aisle -> { item | aisle = Just aisle }

        Just aisle ->
            case aisle of
                None ->
                    { item | aisle = Just (Number 1) }

                Number n ->
                    { item | aisle = Just (Number (n + 1)) }


decrementItemAisle : Maybe Aisle -> { a | aisle : Maybe Aisle } -> { a | aisle : Maybe Aisle }
decrementItemAisle lastAisle item =
    case item.aisle of
        Nothing ->
            case lastAisle of
                Nothing -> { item | aisle = Just None }
                Just aisle -> { item | aisle = Just aisle }

        Just aisle ->
            case aisle of
                None ->
                    item

                Number 0 ->
                    item

                Number 1 ->
                    { item | aisle = Just None }

                Number n ->
                    { item | aisle = Just (Number (n - 1)) }


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


setItemPurchasedState : a -> b -> List { c | id : a, purchased : b } -> List { c | id : a, purchased : b }
setItemPurchasedState id state itemList =
    List.map
        (\i ->
            if i.id == id then
                { i | purchased = state }
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
        [ useSvg
        , h1 [] [ text "Shopping list" ]
        , viewSelector model.currentView
        , case model.items of
            NotAsked ->
                h2 [] [ text "Should never see this" ]

            Loading ->
                h2 [] [ text "Loading items..." ]

            Failure err ->
                h2 [] [ text ("HTTP Failure: " ++ (toString err)) ]

            Success items ->
                successView model
        ]


successView : Model -> Html Msg
successView model =
    let
        items =
            RemoteData.withDefault [] model.items
    in
        case model.currentView of
            ItemList ->
                itemsView model.inputText items

            ShoppingList ->
                shoppingView items

            Checkout ->
                checkoutView items


itemsView : String -> List Item -> Html Msg
itemsView inputText items =
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
        , td [] 
            [ text item.name
            , a [] [ iconWithOptions edit FontAwesome.Regular [ FontAwesome.HasFixedWidth ] [] ]
            ] 
        , td [] [ input [ type_ "checkbox", (checked item.required), onCheck (ToggleRequired item.id) ] [] ]
        ]


shoppingView : List Item -> Html Msg
shoppingView items =
    List.filter (\i -> i.required) items
        |> List.sortWith shoppingCompare
        |> shoppingListView


shoppingCompare : Item -> Item -> Order
shoppingCompare a b =
    let
        comp =
            boolCompare a.purchased b.purchased
    in
        case comp of
            EQ ->
                case a.aisle of
                    Nothing ->
                        case b.aisle of
                            Nothing ->
                                EQ

                            Just ba ->
                                LT

                    Just aa ->
                        case b.aisle of
                            Nothing ->
                                GT

                            Just ba ->
                                let
                                    aisleComp =
                                        aisleCompare aa ba
                                in
                                    case aisleComp of
                                        EQ ->
                                            compare (String.toLower a.name) (String.toLower b.name)

                                        _ ->
                                            aisleComp

            _ ->
                comp


boolCompare : Bool -> Bool -> Order
boolCompare a b =
    if not a then
        if not b then
            EQ
        else
            LT
    else if not b then
        GT
    else
        EQ


shoppingListView : List Item -> Html Msg
shoppingListView items =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Purchased?" ]
                , th [] [ text "Item Name" ]
                , th [] [ text "Aisle" ]
                ]
            ]
        , Keyed.node "tbody" [] (List.map (\i -> ( (toString i.id), (shoppingItemView i) )) items)
        ]


shoppingItemView : Item -> Html Msg
shoppingItemView item =
    let
        textDecor =
            if item.purchased then
                "line-through"
            else
                "none"

        nameStyle =
            style [ ( "text-decoration", textDecor ) ]
    in
        tr []
            [ td [] [ input [ type_ "checkbox", (checked item.purchased), onCheck (TogglePurchased item.id) ] [] ]
            , td [ nameStyle ] [ text item.name ]
            , td [] [ button [ onClick (DecAisle item.id) ] [ text "-" ], (aisleView item.aisle), button [ onClick (IncAisle item.id) ] [ text "+" ] ]
            ]


aisleView : Maybe Aisle -> Html msg
aisleView aisle =
    let
        aisleText =
            case aisle of
                Nothing ->
                    " ? "

                Just a ->
                    case a of
                        None ->
                            " - "

                        Number n ->
                            toString n
    in
        text aisleText


checkoutView : List Item -> Html Msg
checkoutView items =
    let
        required =
            items |> List.filter (\i -> i.required) |> List.length

        purchased =
            items |> List.filter (\i -> i.required && i.purchased) |> List.length
    in
        div []
            [ h2 [] [ text ("Purchased: " ++ (toString purchased) ++ " of " ++ (toString required)) ]
            , hr [] []
            , button [ onClick DoCheckout ] [ text "Do Checkout" ]
            ]


viewSelector : Views -> Html Msg
viewSelector currentView =
    fieldset []
        [ radio (SelectView ItemList) "Pick Items" (currentView == ItemList)
        , radio (SelectView ShoppingList) "Shop!" (currentView == ShoppingList)
        , radio (SelectView Checkout) "Checkout" (currentView == Checkout)
        ]


radio : msg -> String -> Bool -> Html msg
radio msg name isChecked =
    label []
        [ input [ type_ "radio", onClick msg, (checked isChecked) ] []
        , text name
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
