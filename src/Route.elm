module Route
    exposing
    ( Route(..)
    , fromUrl
    , toUrlString
    )

import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser
    exposing
    ( Parser
    , (</>)
    , map
    , s
    , oneOf
    , string
    , top
    )

type Route
    = Auth
    | Lobby
    | Game String

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Auth (s "login")
        , map Lobby top
        , map Game (s "game" </> string)
        ]

fromUrl : Url -> Maybe Route
fromUrl url = Url.Parser.parse routeParser url

toUrlString : Route -> String
toUrlString route =
    case route of
        Auth ->
            absolute [ "login" ] []

        Lobby ->
            absolute [] []

        Game gameId ->
            absolute [ "game" , gameId ] []
        
