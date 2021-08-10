module Users exposing (Model, Msg(..), User, init, update, userDecoder, view)

import Html exposing (Html, a, div, h2, li, section, span, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onMouseOver)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Routes exposing (baseURL)



---- MODEL ----


type alias Model =
    { users : List User }


type alias User =
    { id : String, name : String, description : String }


init : ( Model, Cmd Msg )
init =
    ( { users = [] }, Http.get { url = Routes.baseURL ++ "user/all?$orderby=_created_at%20desc&$limit=20", expect = Http.expectJson GotUsers usersDecoder } )



---- UPDATE ----


type Msg
    = GotUsers (Result Http.Error (List User))


update msg model =
    case msg of
        GotUsers (Ok users) ->
            ( { model | users = users }, Cmd.none )

        GotUsers (Err _) ->
            ( model, Cmd.none )


usersDecoder : Decoder (List User)
usersDecoder =
    list userDecoder


userDecoder : Decoder User
userDecoder =
    Decode.succeed User |> required "id" string |> required "name" string |> required "description" string



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ ul [] (List.map viewUser model.users) ]


viewUser user =
    li []
        [ div [] [ text user.id ]
        , div [] [ text user.name ]
        , div [] [ text user.description ]
        ]
