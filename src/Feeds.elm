module Feeds exposing (Model, Msg(..), init, title, update, view)

import Html exposing (Html, a, button, div, h2, li, span, text, ul)
import Html.Attributes exposing (disabled, href)
import Html.Events exposing (onClick, onMouseOver)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Routes exposing (baseURL)
import Users exposing (User, userDecoder)



---- MODEL ----


type alias Model =
    { feeds : List Feed, user : Maybe FeedUser, readableMore : Bool, notFound : Bool }


type alias Feed =
    { text : String, createdAt : String, user : FeedUser }


type FeedUser
    = UserId String
    | UserData (Maybe User)


init : Maybe String -> ( Model, Cmd Msg )
init userId =
    let
        user =
            Maybe.map UserId userId
    in
    ( { feeds = [], user = user, readableMore = False, notFound = False }
    , Cmd.batch
        [ Http.get
            { url = Routes.baseURL ++ "text/all?$orderby=_created_at%20desc&$limit=20" ++ toFilterUserIdQuery user
            , expect = Http.expectJson GotFeeds feedsDecoder
            }
        , getUser user
        ]
    )



---- UPDATE ----


type Msg
    = GotFeeds (Result Http.Error (List Feed))
    | GetFeeds Int
    | GetUser FeedUser
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
            , Http.get
                { url = Routes.baseURL ++ "text/all?$orderby=_created_at%20desc&$limit=20&$skip=" ++ String.fromInt readed ++ toFilterUserIdQuery model.user
                , expect = Http.expectJson GotFeeds feedsDecoder
                }
            )

        GetUser user ->
            ( model, getUser (Just user) )

        GotUser _ (Ok user) ->
            ( { model | feeds = List.map (\feed -> { feed | user = replaceFeedUser feed.user user }) model.feeds, user = Maybe.map (\u -> replaceFeedUser u user) model.user }, Cmd.none )

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


feedsDecoder : Decoder (List Feed)
feedsDecoder =
    let
        toFeed text createdAt userId =
            Feed text createdAt (UserId userId)
    in
    list (Decode.succeed toFeed |> required "text" string |> required "_created_at" string |> required "_user_id" string)


toFilterUserIdQuery : Maybe FeedUser -> String
toFilterUserIdQuery user =
    case user of
        Just (UserId userId) ->
            "&$filter=_user_id%20eq%20'" ++ userId ++ "'"

        Just (UserData (Just data)) ->
            "&$filter=_user_id%20eq%20'" ++ data.id ++ "'"

        _ ->
            ""


getUser : Maybe FeedUser -> Cmd Msg
getUser user =
    case user of
        Just (UserId userId) ->
            Http.get { url = Routes.baseURL ++ "user/" ++ userId, expect = Http.expectJson (GotUser userId) Users.userDecoder }

        _ ->
            Cmd.none



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (title model) ]
        , ul [] (List.map viewFeed model.feeds)
        , button [ disabled (not model.readableMore), onClick (GetFeeds (List.length model.feeds)) ] [ text "もっと読む" ]
        ]


viewFeed : Feed -> Html Msg
viewFeed feed =
    li [ onMouseOver (GetUser feed.user) ] [ div [] [ text feed.text, viewUser feed.user ], div [] [ text feed.createdAt ] ]


viewUser : FeedUser -> Html Msg
viewUser user =
    case user of
        UserData (Just data) ->
            a [ Routes.href (Routes.UserFeeds data.id) ] [ text ("(@" ++ data.name ++ ")") ]

        UserData Nothing ->
            text "(@匿名ユーザー)"

        UserId userId ->
            text ""


title : Model -> String
title model =
    case model.user of
        Just (UserData (Just data)) ->
            data.name ++ "のつぶやき一覧"

        _ ->
            "つぶやき一覧"
