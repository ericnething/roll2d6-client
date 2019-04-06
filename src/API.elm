{-
Roll2d6 Virtual Tabletop Project

Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program. If not, see
<https://www.gnu.org/licenses/>.
-}

module API
    exposing
    ( register
    , login
    , logout
    , getAllGames
    , newGame
    , createInvite
    , joinGame
    , getPlayers
    , removePlayer
    , getMyPlayerInfo
    , generateNewSheetId
    , updateGameTitle
    )

import Http
import Json.Decode exposing (decodeValue)
import Json.Encode
import Main.Types as Main
import Lobby.Types as Lobby
import Login.Types as Login
import Game.Types as Game
import Game.Sheets.Types as Sheets
import Game.Sheet.Types as Sheet
import Game.Person as Person
import Invite
import Game.Decode
    exposing
        ( decodeGameId
        , decodeGameList
        , gameIdDecoder
        , gameListDecoder
        , playerListDecoder
        , playerDecoder
        )
import Game.Encode
    exposing
    ( encodeGameData
    )
import RemoteData exposing (RemoteData(..), WebData)

domain =
    "/api"


getAllGames : Cmd Lobby.Msg
getAllGames =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url = domain ++ "/games"
                , body = Http.emptyBody
                , expect = Http.expectJson gameListDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        RemoteData.sendRequest request
            |> Cmd.map Lobby.SetGameList


newGame : Game.GameData -> Cmd Lobby.Msg
newGame gameData =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/games"
                , body = Http.jsonBody (encodeGameData gameData)
                , expect = Http.expectJson gameIdDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        RemoteData.sendRequest request
            |> Cmd.map Lobby.NewGameResponse


login : Login.Auth -> Cmd Login.ConsumerMsg
login auth =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/login"
                , body = Http.jsonBody (Login.encodeAuth auth)
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Login.LoginResponse request
            |> Cmd.map Login.LocalMsg


register : Login.Registration -> Cmd Login.ConsumerMsg
register reg =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/register"
                , body = Http.jsonBody (Login.encodeRegistration reg)
                , expect = Http.expectJson Json.Decode.int
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Login.RegisterResponse request
            |> Cmd.map Login.LocalMsg


logout : Cmd Lobby.Msg
logout =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/logout"
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Lobby.LogoutResponse request

createInvite : Game.GameId -> Cmd Game.Msg
createInvite gameId =
    let
        request =
            Http.request
                { method = "PUT"
                , headers = []
                , url = domain ++ "/games/" ++ gameId ++ "/invite"
                , body = Http.emptyBody
                , expect = Http.expectJson Json.Decode.string
                , timeout = Nothing
                , withCredentials = False
                }
    in
        RemoteData.sendRequest request
            |> Cmd.map Game.InviteCreated

joinGame : String -> Cmd Invite.Msg
joinGame inviteId =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/invite/" ++ inviteId
                , body = Http.emptyBody
                , expect = Http.expectJson gameIdDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (Invite.JoinGame inviteId) request

getPlayers : Game.GameId -> Cmd Main.Msg
getPlayers gameId =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url = domain ++ "/games/" ++ gameId ++ "/players"
                , body = Http.emptyBody
                , expect = Http.expectJson playerListDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Main.PlayerListLoaded request

removePlayer : Game.GameId -> Person.PersonId -> Cmd Game.Msg
removePlayer gameId playerId =
    let
        request =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = domain ++ "/games/" ++ gameId
                        ++ "/players/" ++ playerId
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (Game.PlayerRemoved gameId playerId) request


getMyPlayerInfo : Game.GameId -> Cmd Main.Msg
getMyPlayerInfo gameId =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url = domain ++ "/games/" ++ gameId
                        ++ "/my-player-info"
                , body = Http.emptyBody
                , expect = Http.expectJson playerDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send Main.MyPlayerInfoLoaded request


generateNewSheetId : Game.GameId -> Sheet.SheetModel -> Cmd Sheets.Msg
generateNewSheetId gameId sheet =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url = domain ++ "/sheet-id"
                , body = Http.emptyBody
                , expect = Http.expectJson Json.Decode.string
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (Sheets.NewSheetId sheet) request


updateGameTitle : Game.GameId -> String -> Cmd Game.Msg
updateGameTitle gameId title =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/games/" ++ gameId ++ "/title"
                , body = Http.jsonBody
                         (Json.Encode.string title)
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (always Game.GameTitleUpdated) request
