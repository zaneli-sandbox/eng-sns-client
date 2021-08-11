module Routes exposing (Route(..), baseURL, href, match)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Route
    = AllFeeds
    | UserFeeds String
    | Users


baseURL =
    "https://versatileapi.herokuapp.com/api/"


routes : Parser (Route -> a) a
routes =
    Parser.oneOf
        [ Parser.map AllFeeds Parser.top
        , Parser.map AllFeeds (Parser.s "feeds")
        , Parser.map UserFeeds (Parser.s "feeds" </> Parser.string)
        , Parser.map Users (Parser.s "users")
        ]


match : Url -> Maybe Route
match url =
    Parser.parse routes url


routeToUrl : Route -> String
routeToUrl route =
    case route of
        AllFeeds ->
            "/feeds"

        UserFeeds userId ->
            "/feeds/" ++ userId

        Users ->
            "/users"


href : Route -> Html.Attribute msg
href route =
    routeToUrl route |> Html.Attributes.href
