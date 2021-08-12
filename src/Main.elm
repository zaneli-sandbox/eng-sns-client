module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Feeds
import Html exposing (Html, a, aside, div, h2, li, section, span, text, ul)
import Html.Attributes exposing (href)
import Routes
import Task
import Time
import Url exposing (Url)
import Users



---- MODEL ----


type alias Model =
    { page : Page, navKey : Navigation.Key, timeZone : Maybe Time.Zone }


init : Url -> Navigation.Key -> ( Model, Cmd Msg )
init url navKey =
    let
        ( initialModel, initialCmd ) =
            setNewPage (Routes.match url) { page = NotFound, navKey = navKey, timeZone = Nothing }
    in
    ( initialModel, Task.attempt (GotTimeZone initialCmd) <| Time.here )


type Page
    = Feeds Feeds.Model
    | Users Users.Model
    | NotFound



---- UPDATE ----


type Msg
    = GotTimeZone (Cmd Msg) (Result String Time.Zone)
    | FeedsMsg Feeds.Msg
    | UsersMsg Users.Msg
    | NewRoute (Maybe Routes.Route)
    | Visit UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.timeZone, model.page ) of
        ( GotTimeZone nextCmd (Ok timeZone), _, _ ) ->
            ( { model | timeZone = Just timeZone }, nextCmd )

        ( GotTimeZone _ (Err _), _, _ ) ->
            ( { model | timeZone = Just Time.utc }, Cmd.none )

        ( _, Nothing, _ ) ->
            ( model, Task.attempt (GotTimeZone <| Task.perform (\() -> msg) <| Task.succeed ()) <| Time.here )

        ( FeedsMsg feedsMsg, _, Feeds feedsModel ) ->
            let
                ( updatedFeedsModel, feedsCmd ) =
                    Feeds.update feedsMsg feedsModel
            in
            ( { model | page = Feeds updatedFeedsModel }, Cmd.map FeedsMsg feedsCmd )

        ( UsersMsg usersMsg, _, Users usersModel ) ->
            let
                ( updatedUsersModel, usersCmd ) =
                    Users.update usersMsg usersModel
            in
            ( { model | page = Users updatedUsersModel }, Cmd.map UsersMsg usersCmd )

        ( NewRoute route, _, _ ) ->
            setNewPage route model

        ( Visit (Browser.Internal url), _, _ ) ->
            ( model, Navigation.pushUrl model.navKey <| Url.toString url )

        _ ->
            ( model, Cmd.none )


setNewPage : Maybe Routes.Route -> Model -> ( Model, Cmd Msg )
setNewPage route model =
    case ( route, model.timeZone ) of
        ( routes, Nothing ) ->
            ( model, Task.attempt (GotTimeZone <| Task.perform NewRoute <| Task.succeed routes) <| Time.here )

        ( Just Routes.AllFeeds, Just timeZone ) ->
            let
                ( feedsInit, feedsCmd ) =
                    Feeds.init timeZone Nothing
            in
            ( { model | page = Feeds feedsInit }, Cmd.map FeedsMsg feedsCmd )

        ( Just (Routes.UserFeeds userId), Just timeZone ) ->
            let
                ( feedsInit, feedsCmd ) =
                    Feeds.init timeZone <| Just userId
            in
            ( { model | page = Feeds feedsInit }, Cmd.map FeedsMsg feedsCmd )

        ( Just Routes.Users, _ ) ->
            let
                ( usersInit, usersCmd ) =
                    Users.init
            in
            ( { model | page = Users usersInit }, Cmd.map UsersMsg usersCmd )

        ( Nothing, _ ) ->
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
                { title = Feeds.pageTitle feedsModel, body = [ viewHeader, content ] }

        Users usersModel ->
            let
                content =
                    Users.view usersModel |> Html.map UsersMsg
            in
            { title = Users.pageTitle, body = [ viewHeader, content ] }

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
