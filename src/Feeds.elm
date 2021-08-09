module Feeds exposing (Model, Msg(..), init, update, view)

import Debug
import Html exposing (Html, a, aside, div, h2, li, section, text, ul)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



---- MODEL ----


type alias Model =
    { feeds : List Feed }


type alias Feed =
    { text : String, createdAt : String, userId : String }


init : ( Model, Cmd Msg )
init =
    ( { feeds = [] }, Http.get { url = "https://versatileapi.herokuapp.com/api/text/all?$orderby=_created_at%20desc&$limit=20", expect = Http.expectJson GotFeeds feedDecoder } )



---- UPDATE ----


type Msg
    = GotFeeds (Result Http.Error (List Feed))


update msg model =
    case msg of
        GotFeeds (Ok feeds) ->
            ( { model | feeds = feeds }, Cmd.none )

        GotFeeds (Err _) ->
            ( model, Cmd.none )


feedDecoder : Decoder (List Feed)
feedDecoder =
    list (Decode.succeed Feed |> required "text" string |> required "_created_at" string |> required "_user_id" string)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "つぶやき一覧" ]
        , ul [] (List.map (\feed -> li [] [ div [] [ text feed.text ], div [] [ text feed.createdAt ] ]) model.feeds)
        ]
