module Feeds exposing (Model, Msg(..), init, update, view)

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
    { feeds : List Feed, userId : Maybe String, readableMore : Bool, notFound : Bool }


type alias Feed =
    { text : String, createdAt : String, user : FeedUser }


type FeedUser
    = UserId String
    | UserData (Maybe User)


init : Maybe String -> ( Model, Cmd Msg )
init userId =
    ( { feeds = [], userId = userId, readableMore = False, notFound = False }
    , Http.get
        { url = Routes.baseURL ++ "text/all?$orderby=_created_at%20desc&$limit=20" ++ toFilterUserIdQuery userId
        , expect = Http.expectJson GotFeeds feedsDecoder
        }
    )



---- UPDATE ----


type Msg
    = GotFeeds (Result Http.Error (List Feed))
    | GetFeeds Int
    | GetUser FeedUser
    | GotUser String (Result Http.Error User)


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
                { url = Routes.baseURL ++ "text/all?$orderby=_created_at%20desc&$limit=20&$skip=" ++ String.fromInt readed ++ toFilterUserIdQuery model.userId
                , expect = Http.expectJson GotFeeds feedsDecoder
                }
            )

        GetUser (UserId userId) ->
            ( model, Http.get { url = Routes.baseURL ++ "user/" ++ userId, expect = Http.expectJson (GotUser userId) Users.userDecoder } )

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


feedsDecoder : Decoder (List Feed)
feedsDecoder =
    let
        toFeed text createdAt userId =
            Feed text createdAt (UserId userId)
    in
    list (Decode.succeed toFeed |> required "text" string |> required "_created_at" string |> required "_user_id" string)


toFilterUserIdQuery userId =
    case userId of
        Just id ->
            "&$filter=_user_id%20eq%20'" ++ id ++ "'"

        Nothing ->
            ""



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ ul [] (List.map viewFeed model.feeds)
        , button [ disabled (not model.readableMore), onClick (GetFeeds (List.length model.feeds)) ] [ text "もっと読む" ]
        ]


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
