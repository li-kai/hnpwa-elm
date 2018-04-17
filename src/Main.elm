module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Html.Keyed
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


---- MODEL ----


type alias Item =
    { id : Int
    , title : String
    , points : Maybe Int
    , user : Maybe String
    , time : Int
    , timeAgo : String
    , comments_count : Int
    , itemType : String
    , url : String
    , domain : String
    }


type alias Items =
    List Item


type alias Model =
    { items : Items
    , page : Int
    , error : String
    }


init : ( Model, Cmd Msg )
init =
    ( { items = [], page = 1, error = "" }, fetchItems 1 )



---- HTTP ----


fetchItems : Int -> Cmd Msg
fetchItems page =
    let
        url =
            "https://api.hnpwa.com/v0/news/" ++ toString page ++ ".json"

        request =
            Http.get url decodeItems
    in
    Http.send NewItems request


decodeItems : Decode.Decoder Items
decodeItems =
    Decode.list decodeItem


decodeItem : Decode.Decoder Item
decodeItem =
    Pipeline.decode Item
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "points" (Decode.nullable Decode.int)
        |> Pipeline.required "user" (Decode.nullable Decode.string)
        |> Pipeline.required "time" Decode.int
        |> Pipeline.required "time_ago" Decode.string
        |> Pipeline.required "comments_count" Decode.int
        |> Pipeline.required "type" Decode.string
        |> Pipeline.required "url" Decode.string
        |> Pipeline.optional "domain" Decode.string "https://news.ycombinator.com"



---- UPDATE ----


type Msg
    = GetItems Int
    | NewItems (Result Http.Error Items)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetItems pageNum ->
            ( model, fetchItems pageNum )

        NewItems (Ok items) ->
            ( { model | items = items }, Cmd.none )

        NewItems (Err e) ->
            ( { model | error = toString e }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewNav
        , main_ [ class "main" ]
            [ if model.error == "" then
                Html.Keyed.ol [ class "items" ] (List.map viewItem model.items)
              else
                div [ class "box error" ] [ text "An error has occurred" ]
            ]
        ]


viewNav : Html msg
viewNav =
    nav [ class "nav" ]
        [ a [ class "nav-logo", href "/" ] [ img [ src "/logo.svg" ] [] ]
        , a [ class "nav-link", href "/" ] [ text "Home" ]
        ]


viewItem : Item -> ( String, Html msg )
viewItem item =
    ( toString item.id
    , li [ class "item-container" ]
        [ article [ class "box item" ]
            [ viewItemHeader item
            , viewItemDetails item
            ]
        ]
    )


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


viewItemDetails : Item -> Html msg
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
        , a [ class "item-comments" ] [ text (toString item.comments_count ++ " comments") ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
