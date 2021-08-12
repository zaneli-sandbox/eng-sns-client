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
    , getUsers Nothing
    )



---- UPDATE ----


type Msg
    = GotUsers (Result Http.Error (List User))
    | GetUsers Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUsers (Ok users) ->
            ( { model | users = model.users ++ users, readableMore = True }, Cmd.none )

        GotUsers (Err _) ->
            ( model, Cmd.none )

        GetUsers readed ->
            ( { model | readableMore = False }
            , getUsers (Just readed)
            )


usersDecoder : Decoder (List User)
usersDecoder =
    list userDecoder


userDecoder : Decoder User
userDecoder =
    Decode.succeed User |> required "id" string |> required "name" string |> required "description" string


getUsers : Maybe Int -> Cmd Msg
getUsers readed =
    let
        skip =
            case readed of
                Just num ->
                    "&$skip=" ++ String.fromInt num

                Nothing ->
                    ""
    in
    Http.get
        { url = Routes.baseURL ++ "user/all?$orderby=_created_at%20desc&$limit=20" ++ skip
        , expect = Http.expectJson GotUsers usersDecoder
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text title ]
        , ul [] (List.map viewUser model.users)
        , button [ disabled <| not model.readableMore, onClick (List.length model.users |> GetUsers) ] [ text "もっと読む" ]
        ]


viewUser : User -> Html Msg
viewUser user =
    li []
        [ div [] [ a [ Routes.href <| Routes.UserFeeds user.id ] [ text user.name ] ]
        , div [] [ text user.description ]
        ]


title : String
title =
    "ユーザー一覧"
