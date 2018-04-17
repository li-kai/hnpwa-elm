module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
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
    , error : String
    }


init : ( Model, Cmd Msg )
init =
    ( { items = [], error = "" }, fetchItems )



---- HTTP ----


fetchItems : Cmd Msg
fetchItems =
    let
        request =
            Http.get "https://api.hnpwa.com/v0/news/1.json" decodeItems
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
    = GetItems
    | NewItems (Result Http.Error Items)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetItems ->
            ( model, fetchItems )

        NewItems (Ok items) ->
            ( { model | items = items }, Cmd.none )

        NewItems (Err e) ->
            ( { model | error = toString e }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Your Elm App is working!" ]
        , if model.error == "" then
            text ""
          else
            div [ class "box error" ] [ text "An error has occurred" ]
        , viewItems model.items
        ]


viewItems : Items -> Html msg
viewItems items =
    Html.Keyed.ol [] (List.map viewItem items)


viewItem : Item -> ( String, Html msg )
viewItem item =
    ( toString item.id
    , li []
        [ article [ class "box card" ]
            [ h2 [] [ a [ href item.url ] [ text item.title ] ]
            , div [] [ viewPost item.points item.user item.timeAgo ]
            ]
        ]
    )


viewPost : Maybe Int -> Maybe String -> String -> Html msg
viewPost pt usr timeAgo =
    let
        reputation =
            case ( pt, usr ) of
                ( Just point, Just user ) ->
                    toString point ++ " points by " ++ user ++ " "

                _ ->
                    ""
    in
    text (reputation ++ timeAgo)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
