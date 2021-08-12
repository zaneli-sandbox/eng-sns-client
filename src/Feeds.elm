module Feeds exposing (Model, Msg(..), init, title, update, view)

import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h2, li, span, text, ul)
import Html.Attributes exposing (disabled, href)
import Html.Events exposing (onClick, onMouseOver)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Routes exposing (baseURL)
import Users exposing (User, userDecoder)



---- MODEL ----


type alias Model =
    { feeds : List Feed
    , user : Maybe FeedUser
    , users : Dict String User
    , readableMore : Bool
    , notFound : Bool
    }


type alias Feed =
    { text : String
    , createdAt : String
    , user : FeedUser
    }


type FeedUser
    = UserId String
    | UserData (Maybe User)


init : Maybe String -> ( Model, Cmd Msg )
init userId =
    let
        user =
            Maybe.map UserId userId
    in
    ( { feeds = [], user = user, users = Dict.empty, readableMore = False, notFound = False }
    , Cmd.batch
        [ getFeeds user Nothing
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
            ( { model | feeds = model.feeds ++ replaceFeedUsers feeds model.users, readableMore = True }, Cmd.none )

        GotFeeds (Err (Http.BadStatus 404)) ->
            if List.isEmpty model.feeds then
                ( { model | notFound = True }, Cmd.none )

            else
                ( model, Cmd.none )

        GotFeeds (Err _) ->
            ( model, Cmd.none )

        GetFeeds readed ->
            ( { model | readableMore = False }
            , getFeeds model.user (Just readed)
            )

        GetUser user ->
            let
                updatedModel =
                    case getCachedUser model.users user of
                        Just cachedUser ->
                            { model | feeds = List.map (\feed -> { feed | user = replaceFeedUser feed.user cachedUser }) model.feeds }

                        Nothing ->
                            model
            in
            ( updatedModel, getUser (Just user) )

        GotUser _ (Ok user) ->
            ( { model
                | feeds = List.map (\feed -> { feed | user = replaceFeedUser feed.user user }) model.feeds
                , user = Maybe.map (\u -> replaceFeedUser u user) model.user
                , users = Dict.insert user.id user model.users
              }
            , Cmd.none
            )

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


replaceFeedUsers : List Feed -> Dict String User -> List Feed
replaceFeedUsers feeds users =
    let
        getUserFromDict feed =
            case feed.user of
                UserId userId ->
                    Dict.get userId users |> Maybe.map (\u -> UserData (Just u))

                _ ->
                    Nothing
    in
    List.map (\feed -> { feed | user = Maybe.withDefault feed.user (getUserFromDict feed) }) feeds


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


getFeeds : Maybe FeedUser -> Maybe Int -> Cmd Msg
getFeeds user readed =
    let
        skip =
            case readed of
                Just num ->
                    "&$skip=" ++ String.fromInt num

                Nothing ->
                    ""
    in
    Http.get
        { url = Routes.baseURL ++ "text/all?$orderby=_created_at%20desc&$limit=20" ++ skip ++ toFilterUserIdQuery user
        , expect = Http.expectJson GotFeeds feedsDecoder
        }


getUser : Maybe FeedUser -> Cmd Msg
getUser user =
    case user of
        Just (UserId userId) ->
            Http.get { url = Routes.baseURL ++ "user/" ++ userId, expect = Http.expectJson (GotUser userId) Users.userDecoder }

        _ ->
            Cmd.none


getCachedUser : Dict String User -> FeedUser -> Maybe User
getCachedUser users user =
    case user of
        UserId userId ->
            Dict.get userId users

        UserData data ->
            data



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (title model) ]
        , ul [] (List.map (lazy viewFeed) model.feeds)
        , button [ disabled (not model.readableMore), onClick (List.length model.feeds |> GetFeeds) ] [ text "もっと読む" ]
        ]


viewFeed : Feed -> Html Msg
viewFeed feed =
    li [ onMouseOver (GetUser feed.user) ] [ div [] [ text feed.text, lazy viewUser feed.user ], div [] [ text feed.createdAt ] ]


viewUser : FeedUser -> Html Msg
viewUser user =
    case user of
        UserData (Just data) ->
            a [ Routes.UserFeeds data.id |> Routes.href ] [ text ("(@" ++ data.name ++ ")") ]

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
