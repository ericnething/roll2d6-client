-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it
-- and/or modify it under the terms of the GNU Affero
-- General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A
-- PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General
-- Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

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
    | Invite String

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Auth (s "login")
        , map Lobby top
        , map Game (s "game" </> string)
        , map Invite (s "invite" </> string)
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

        Invite inviteId ->
            absolute [ "invite" , inviteId ] []
        
