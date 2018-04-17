module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Html.Keyed
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


---- MODEL ----


type Comments
    = Comments Items
    | Empty


type alias Item =
    { id : Int
    , title : String
    , points : Maybe Int
    , user : Maybe String
    , time : Int
    , timeAgo : String
    , comments_count : Int
    , comments : Comments
    , content : String
    , itemType : String
    , url : String
    , domain : String
    }


type alias Items =
    List Item


type alias Model =
    { items : Items
    , item : Maybe Item
    , page : Int
    , error : String
    }


init : ( Model, Cmd Msg )
init =
    ( { items = [], item = Nothing, page = 1, error = "" }, fetchItems 1 )



---- HTTP ----


endpoint : String
endpoint =
    "https://api.hnpwa.com/v0"


fetchItems : Int -> Cmd Msg
fetchItems page =
    let
        url =
            endpoint ++ "/news/" ++ toString page ++ ".json"

        request =
            Http.get url decodeItems
    in
    Http.send NewItems request


fetchItem : Int -> Cmd Msg
fetchItem id =
    let
        url =
            endpoint ++ "/item/" ++ toString id ++ ".json"

        request =
            Http.get url decodeItem
    in
    Http.send NewItem request


decodeItems : Decode.Decoder Items
decodeItems =
    Decode.list decodeItem


decodeItem : Decode.Decoder Item
decodeItem =
    Pipeline.decode Item
        |> Pipeline.required "id" Decode.int
        |> Pipeline.optional "title" Decode.string ""
        |> Pipeline.optional "points" (Decode.nullable Decode.int) Nothing
        |> Pipeline.optional "user" (Decode.nullable Decode.string) Nothing
        |> Pipeline.required "time" Decode.int
        |> Pipeline.required "time_ago" Decode.string
        |> Pipeline.required "comments_count" Decode.int
        |> Pipeline.optional "comments" (Decode.lazy (\_ -> decodeComments)) Empty
        |> Pipeline.optional "content" Decode.string ""
        |> Pipeline.required "type" Decode.string
        |> Pipeline.required "url" Decode.string
        |> Pipeline.optional "domain" Decode.string "https://news.ycombinator.com"


decodeComments : Decode.Decoder Comments
decodeComments =
    Decode.map Comments (Decode.list (Decode.lazy (\_ -> decodeItem)))



---- UPDATE ----


type Msg
    = GetItems Int
    | GetItem Int
    | NewItems (Result Http.Error Items)
    | NewItem (Result Http.Error Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetItems pageNum ->
            ( model, fetchItems pageNum )

        GetItem id ->
            ( model, fetchItem id )

        NewItems (Ok items) ->
            ( { model | items = items }, Cmd.none )

        NewItem (Ok item) ->
            ( { model | item = Just item }, Cmd.none )

        NewItem (Err e) ->
            ( { model | item = Nothing, error = toString e }, Cmd.none )

        NewItems (Err e) ->
            ( { model | error = toString e }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewNav
        , main_ [ class "main" ]
            [ case ( model.item, model.error ) of
                ( Nothing, "" ) ->
                    Html.Keyed.ol [ class "items" ] (List.map viewListItem model.items)

                ( Just item, "" ) ->
                    viewItem item

                ( _, err ) ->
                    div [ class "box error" ] [ text err ]
            ]
        ]


viewNav : Html msg
viewNav =
    nav [ class "nav" ]
        [ a [ class "nav-logo", href "/" ] [ img [ src "/logo.svg" ] [] ]
        , a [ class "nav-link", href "/" ] [ text "Home" ]
        ]


viewListItem : Item -> ( String, Html Msg )
viewListItem item =
    ( toString item.id
    , li [ class "item-container" ]
        [ viewItem item
        ]
    )


viewItem : Item -> Html Msg
viewItem item =
    article [ class "box item" ]
        [ viewItemHeader item
        , viewItemDetails item
        , viewItemComments item.comments
        ]


viewItemHeader : Item -> Html msg
viewItemHeader item =
    div [ class "item-header" ]
        [ h2 [ class "item-title" ]
            [ a [ class "item-url", href item.url ]
                [ text item.title ]
            ]
        , small [ class "item-domain" ]
            [ text item.domain ]
        ]


viewItemDetails : Item -> Html Msg
viewItemDetails item =
    let
        reputation =
            case ( item.points, item.user ) of
                ( Just points, Just user ) ->
                    toString points ++ " points by " ++ user ++ " "

                _ ->
                    ""
    in
    div [ class "item-details" ]
        [ span [ class "item-poster" ] [ text (reputation ++ item.timeAgo) ]
        , a [ class "item-comments", onClick (GetItem item.id) ] [ text (toString item.comments_count ++ " comments") ]
        ]


viewItemComments : Comments -> Html Msg
viewItemComments comments =
    ul [ class "comments" ] (List.map viewComment (getComments comments))


viewComment : Item -> Html Msg
viewComment item =
    li []
        [ div [ class "comment-meta" ]
            [ text (Maybe.withDefault "" item.user ++ " " ++ item.timeAgo)
            ]
        , rawHtml div item.content
        , viewItemComments item.comments
        ]


getComments : Comments -> List Item
getComments comments =
    case comments of
        Comments items ->
            items

        Empty ->
            []


rawHtml : (List (Attribute Msg) -> List (Html Msg) -> Html Msg) -> String -> Html Msg
rawHtml node htmlString =
    node [ Html.Attributes.property "innerHTML" (Encode.string htmlString) ] []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
