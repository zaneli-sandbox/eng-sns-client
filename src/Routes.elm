module Routes exposing (Route(..), baseURL, href, match)

import Html
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Feeds
    | Users


baseURL =
    "https://versatileapi.herokuapp.com/api/"


routes : Parser (Route -> a) a
routes =
    Parser.oneOf [ Parser.map Feeds Parser.top, Parser.map Feeds (Parser.s "feeds"), Parser.map Users (Parser.s "users") ]


match : Url -> Maybe Route
match url =
    Parser.parse routes url


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Feeds ->
            "/feeds"

        Users ->
            "/users"


href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (routeToUrl route)
