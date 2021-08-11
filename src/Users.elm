module Users exposing (Model, Msg(..), User, init, title, update, userDecoder, view)

import Html exposing (Html, a, button, div, h2, li, span, text, ul)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Routes exposing (baseURL)



---- MODEL ----


type alias Model =
    { users : List User, readableMore : Bool }


type alias User =
    { id : String, name : String, description : String }


init : ( Model, Cmd Msg )
init =
    ( { users = [], readableMore = False }
    , Http.get { url = Routes.baseURL ++ "user/all?$orderby=_created_at%20desc&$limit=20&", expect = Http.expectJson GotUsers usersDecoder }
    )



---- UPDATE ----


type Msg
    = GotUsers (Result Http.Error (List User))
    | GetUsers Int


update msg model =
    case msg of
        GotUsers (Ok users) ->
            ( { model | users = model.users ++ users, readableMore = True }, Cmd.none )

        GotUsers (Err _) ->
            ( model, Cmd.none )

        GetUsers readed ->
            ( { model | readableMore = False }
            , Http.get { url = Routes.baseURL ++ "user/all?$orderby=_created_at%20desc&$limit=20&$skip=" ++ String.fromInt readed, expect = Http.expectJson GotUsers usersDecoder }
            )


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
        [ h2 [] [ text title ]
        , ul [] (List.map viewUser model.users)
        , button [ disabled (not model.readableMore), onClick (GetUsers (List.length model.users)) ] [ text "もっと読む" ]
        ]


viewUser user =
    li []
        [ div [] [ a [ Routes.href (Routes.UserFeeds user.id) ] [ text user.name ] ]
        , div [] [ text user.description ]
        ]


title =
    "ユーザー一覧"
