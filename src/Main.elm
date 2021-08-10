module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Feeds
import Html exposing (Html, a, aside, div, h2, li, section, span, text, ul)
import Html.Attributes exposing (href)
import Routes
import Url exposing (Url)
import Users



---- MODEL ----


type alias Model =
    { page : Page, navKey : Navigation.Key }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url navKey =
    setNewPage (Routes.match url) (initialModel navKey)


type Page
    = Feeds Feeds.Model
    | Users Users.Model
    | NotFound


initialModel navKey =
    { page = NotFound, navKey = navKey }



---- UPDATE ----


type Msg
    = FeedsMsg Feeds.Msg
    | UsersMsg Users.Msg
    | NewRoute (Maybe Routes.Route)
    | Visit UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( FeedsMsg feedsMsg, Feeds feedsModel ) ->
            let
                ( updatedFeedsModel, feedsCmd ) =
                    Feeds.update feedsMsg feedsModel
            in
            ( { model | page = Feeds updatedFeedsModel }, Cmd.map FeedsMsg feedsCmd )

        ( UsersMsg usersMsg, Users usersModel ) ->
            let
                ( updatedUsersModel, usersCmd ) =
                    Users.update usersMsg usersModel
            in
            ( { model | page = Users updatedUsersModel }, Cmd.map UsersMsg usersCmd )

        ( NewRoute route, _ ) ->
            setNewPage route model

        ( Visit (Browser.Internal url), _ ) ->
            ( model, Navigation.pushUrl model.navKey (Url.toString url) )

        _ ->
            ( model, Cmd.none )


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage route model =
    case route of
        Just Routes.AllFeeds ->
            let
                ( feedsInit, feedsCmd ) =
                    Feeds.init Nothing
            in
            ( { model | page = Feeds feedsInit }, Cmd.map FeedsMsg feedsCmd )

        Just (Routes.UserFeeds userId) ->
            let
                ( feedsInit, feedsCmd ) =
                    Feeds.init (Just userId)
            in
            ( { model | page = Feeds feedsInit }, Cmd.map FeedsMsg feedsCmd )

        Just Routes.Users ->
            let
                ( usersInit, usersCmd ) =
                    Users.init
            in
            ( { model | page = Users usersInit }, Cmd.map UsersMsg usersCmd )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    case model.page of
        Feeds feedsModel ->
            if feedsModel.notFound then
                { title = "ページが見つかりません", body = [ viewHeader, text "ページが見つかりません" ] }

            else
                let
                    content =
                        Feeds.view feedsModel |> Html.map FeedsMsg
                in
                { title = "つぶやき一覧", body = [ viewHeader, content ] }

        Users usersModel ->
            let
                content =
                    Users.view usersModel |> Html.map UsersMsg
            in
            { title = "ユーザー一覧", body = [ viewHeader, content ] }

        NotFound ->
            { title = "ページが見つかりません", body = [ viewHeader, text "ページが見つかりません" ] }


viewHeader =
    h2 []
        [ span [] [ a [ Routes.href Routes.AllFeeds ] [ text "つぶやき一覧" ] ]
        , span [] [ text "\u{00A0}" ]
        , span [] [ a [ Routes.href Routes.Users ] [ text "ユーザー一覧" ] ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = Visit
        , onUrlChange = Routes.match >> NewRoute
        }
