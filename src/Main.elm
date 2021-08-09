module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Feeds
import Html exposing (Html, a, aside, div, li, section, text, ul)



---- MODEL ----


type alias Model =
    { page : Page }


init : ( Model, Cmd Msg )
init =
    let
        ( feedsInit, feedsCmd ) =
            Feeds.init
    in
    ( { page = Feeds feedsInit }, Cmd.map FeedsMsg feedsCmd )


type Page
    = Feeds Feeds.Model



---- UPDATE ----


type Msg
    = FeedsMsg Feeds.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( FeedsMsg feedsMsg, Feeds feedsModel ) ->
            let
                ( updatedFeedsModel, feedsCmd ) =
                    Feeds.update feedsMsg feedsModel
            in
            ( { model | page = Feeds updatedFeedsModel }, Cmd.map FeedsMsg feedsCmd )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.page of
        Feeds feedsModel ->
            Feeds.view feedsModel |> Html.map FeedsMsg



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
