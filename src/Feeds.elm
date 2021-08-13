module Feeds exposing (Model, Msg(..), init, pageTitle, update, view)

import DateFormat
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h2, li, span, text, ul)
import Html.Attributes exposing (disabled, href, title)
import Html.Events exposing (onClick, onMouseOver)
import Html.Lazy exposing (lazy, lazy4)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Routes exposing (baseURL)
import Time
import Users exposing (User, anonymous, userDecoder)



---- MODEL ----


type alias Model =
    { feeds : List Text
    , userId : Maybe String
    , texts : Dict String Text
    , users : Dict String User
    , timeZone : Time.Zone
    , readableMore : Bool
    , notFound : Bool
    }


type alias Text =
    { id : String
    , text : String
    , createdAt : Time.Posix
    , userId : String
    , replyTo : Maybe String
    }


init : Time.Zone -> Maybe String -> ( Model, Cmd Msg )
init timeZone userId =
    ( { feeds = [], userId = userId, texts = Dict.empty, users = Dict.empty, timeZone = timeZone, readableMore = False, notFound = False }
    , Cmd.batch
        [ getFeeds userId Nothing
        , getUser userId
        ]
    )



---- UPDATE ----


type Msg
    = GotFeeds (Result Http.Error (List Text))
    | GetFeeds Int
    | GotFeed (Result Http.Error Text)
    | GetFeed String
    | GetUser String
    | GotUser String (Result Http.Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFeeds (Ok feeds) ->
            ( { model | feeds = model.feeds ++ feeds, readableMore = True }, Cmd.none )

        GotFeeds (Err (Http.BadStatus 404)) ->
            if List.isEmpty model.feeds then
                ( { model | notFound = True }, Cmd.none )

            else
                ( model, Cmd.none )

        GotFeeds (Err _) ->
            ( model, Cmd.none )

        GetFeeds readed ->
            ( { model | readableMore = False }
            , getFeeds model.userId <| Just readed
            )

        GotFeed (Ok text) ->
            ( { model | texts = Dict.insert text.id text model.texts }, Cmd.none )

        GotFeed (Err _) ->
            ( model, Cmd.none )

        GetFeed textId ->
            let
                cmd =
                    case Dict.get textId model.texts of
                        Nothing ->
                            Http.get
                                { url = Routes.baseURL ++ "text/" ++ textId
                                , expect = Http.expectJson GotFeed textDecoder
                                }

                        Just _ ->
                            Cmd.none
            in
            ( model, cmd )

        GetUser userId ->
            let
                cmd =
                    case Dict.get userId model.users of
                        Nothing ->
                            getUser (Just userId)

                        Just _ ->
                            Cmd.none
            in
            ( model, cmd )

        GotUser _ (Ok user) ->
            ( { model
                | users = Dict.insert user.id user model.users
              }
            , Cmd.none
            )

        GotUser userId (Err (Http.BadStatus 404)) ->
            ( { model | users = Dict.insert userId (anonymous userId) model.users }, Cmd.none )

        GotUser _ (Err _) ->
            ( model, Cmd.none )


textsDecoder : Decoder (List Text)
textsDecoder =
    list textDecoder


textDecoder : Decoder Text
textDecoder =
    let
        toText id text createdAt userId replyTo =
            case replyTo of
                "" ->
                    Text id text createdAt userId Nothing

                textId ->
                    Text id text createdAt userId (Just textId)
    in
    Decode.succeed toText
        |> required "id" string
        |> required "text" string
        |> required "_created_at" Iso8601.decoder
        |> required "_user_id" string
        |> optional "in_reply_to_text_id" string ""


getFeeds : Maybe String -> Maybe Int -> Cmd Msg
getFeeds userId readed =
    let
        skip =
            case readed of
                Just num ->
                    "&$skip=" ++ String.fromInt num

                Nothing ->
                    ""

        user =
            userId
                |> Maybe.map (\id -> "&$filter=_user_id%20eq%20'" ++ id ++ "'")
                |> Maybe.withDefault ""
    in
    Http.get
        { url = Routes.baseURL ++ "text/all?$orderby=_created_at%20desc&$limit=20" ++ skip ++ user
        , expect = Http.expectJson GotFeeds textsDecoder
        }


getUser : Maybe String -> Cmd Msg
getUser userId =
    userId
        |> Maybe.map (\id -> Http.get { url = Routes.baseURL ++ "user/" ++ id, expect = Http.expectJson (GotUser id) Users.userDecoder })
        |> Maybe.withDefault Cmd.none



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text <| pageTitle model ]
        , ul [] (List.map (lazy4 viewFeed model.timeZone model.texts model.users) model.feeds)
        , button [ disabled <| not model.readableMore, onClick (List.length model.feeds |> GetFeeds) ] [ text "もっと読む" ]
        ]


viewFeed : Time.Zone -> Dict String Text -> Dict String User -> Text -> Html Msg
viewFeed timeZone texts users feed =
    li [ onMouseOver <| GetUser feed.userId ]
        [ div [] (viewReplyTo feed.replyTo texts)
        , div [] [ text feed.text, lazy viewUser <| Dict.get feed.userId users ]
        , div [] [ text <| timeFormatter timeZone feed.createdAt ]
        ]


viewReplyTo replyTo texts =
    case replyTo of
        Just textId ->
            let
                showReply =
                    Dict.get textId texts |> Maybe.map (\r -> [ title r.text ]) |> Maybe.withDefault []
            in
            [ a ((onMouseOver <| GetFeed textId) :: showReply) [ text <| ">> " ++ textId ++ " への返信" ] ]

        Nothing ->
            []


timeFormatter : Time.Zone -> Time.Posix -> String
timeFormatter =
    DateFormat.format
        [ DateFormat.yearNumber
        , DateFormat.text "/"
        , DateFormat.monthFixed
        , DateFormat.text "/"
        , DateFormat.dayOfMonthFixed
        , DateFormat.text " "
        , DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        ]


viewUser : Maybe User -> Html Msg
viewUser user =
    user
        |> Maybe.map (\u -> a [ Routes.href <| Routes.UserFeeds u.id ] [ text <| "(@" ++ u.name ++ ")" ])
        |> Maybe.withDefault (text "")


pageTitle : Model -> String
pageTitle model =
    model.userId
        |> Maybe.andThen (\userId -> Dict.get userId model.users)
        |> Maybe.map (\user -> user.name ++ "のつぶやき一覧")
        |> Maybe.withDefault "つぶやき一覧"
