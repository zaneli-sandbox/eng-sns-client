module Feeds exposing (Model, Msg(..), init, update, view)

import Debug
import Html exposing (Html, a, div, h2, li, section, text, ul)
import Html.Events exposing (onMouseOver)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



---- MODEL ----


baseURL =
    "https://versatileapi.herokuapp.com/api/"


type alias Model =
    { feeds : List Feed }


type alias Feed =
    { text : String, createdAt : String, user : FeedUser }


type FeedUser
    = UserId String
    | UserData (Maybe User)


type alias User =
    { id : String, name : String, description : String }


init : ( Model, Cmd Msg )
init =
    ( { feeds = [] }, Http.get { url = baseURL ++ "text/all?$orderby=_created_at%20desc&$limit=20", expect = Http.expectJson GotFeeds feedDecoder } )



---- UPDATE ----


type Msg
    = GotFeeds (Result Http.Error (List Feed))
    | GetUser FeedUser
    | GotUser String (Result Http.Error User)


update msg model =
    case msg of
        GotFeeds (Ok feeds) ->
            ( { model | feeds = feeds }, Cmd.none )

        GotFeeds (Err _) ->
            ( model, Cmd.none )

        GetUser (UserId userId) ->
            ( model, Http.get { url = baseURL ++ "user/" ++ userId, expect = Http.expectJson (GotUser userId) userDecoder } )

        GetUser (UserData user) ->
            ( model, Cmd.none )

        GotUser _ (Ok user) ->
            ( { model | feeds = List.map (\feed -> { feed | user = replaceFeedUser feed.user user }) model.feeds }, Cmd.none )

        GotUser userId (Err (Http.BadStatus 404)) ->
            ( { model | feeds = List.map (\feed -> { feed | user = replaceAnonymousFeedUser feed.user userId }) model.feeds }, Cmd.none )

        GotUser _ (Err _) ->
            ( model, Cmd.none )


replaceFeedUser : FeedUser -> User -> FeedUser
replaceFeedUser feedUser user =
    case feedUser of
        UserId userId ->
            if userId == user.id then
                UserData (Just user)

            else
                UserId userId

        UserData _ ->
            feedUser


replaceAnonymousFeedUser : FeedUser -> String -> FeedUser
replaceAnonymousFeedUser feedUser anonymousUserId =
    case feedUser of
        UserId userId ->
            if userId == anonymousUserId then
                UserData Nothing

            else
                UserId userId

        UserData _ ->
            feedUser


feedDecoder : Decoder (List Feed)
feedDecoder =
    let
        toFeed text createdAt userId =
            Feed text createdAt (UserId userId)
    in
    list (Decode.succeed toFeed |> required "text" string |> required "_created_at" string |> required "_user_id" string)


userDecoder : Decoder User
userDecoder =
    Decode.succeed User |> required "id" string |> required "name" string |> required "description" string



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "つぶやき一覧" ]
        , ul [] (List.map (\feed -> li [ onMouseOver (GetUser feed.user) ] ([ div [] [ text feed.text ], div [] [ text feed.createdAt ] ] ++ viewUser feed.user)) model.feeds)
        ]


viewUser : FeedUser -> List (Html msg)
viewUser user =
    case user of
        UserData (Just data) ->
            [ div [] [ text data.name ] ]

        UserData Nothing ->
            [ div [] [ text "匿名ユーザー" ] ]

        UserId userId ->
            []