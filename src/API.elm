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


module API exposing
    ( generateNewSheetId
    , getAllGames
    , getMyPlayerInfo
    , getPlayers
    , login
    , logout
    , newGame
    , register
    , removePlayer
    , updateGameTitle
    )

import App.Types as App
import Game.Decode
    exposing
        ( decodeGameId
        , decodeGameList
        , gameIdDecoder
        , gameListDecoder
        , playerDecoder
        , playerListDecoder
        )
import Game.Encode
    exposing
        ( encodeGameData
        )
import Game.Player as Player
import Game.Sheet.Types as Sheet
import Game.Sheets.Types as Sheets
import Game.Types as Game
import Http
import Invite
import Json.Decode exposing (decodeValue)
import Json.Encode
import Lobby.Types as Lobby
import Login.Types as Login
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


login : Login.Auth -> Cmd Login.Msg
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


register : Login.Registration -> Cmd Login.Msg
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


getPlayers : Game.GameId -> Cmd App.Msg
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
    Http.send App.PlayerListLoaded request


removePlayer : Game.GameId -> Player.PlayerId -> Cmd Game.Msg
removePlayer gameId playerId =
    let
        request =
            Http.request
                { method = "DELETE"
                , headers = []
                , url =
                    domain
                        ++ "/games/"
                        ++ gameId
                        ++ "/players/"
                        ++ playerId
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send (Game.PlayerRemoved gameId playerId) request


getMyPlayerInfo : Game.GameId -> Cmd App.Msg
getMyPlayerInfo gameId =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , url =
                    domain
                        ++ "/games/"
                        ++ gameId
                        ++ "/my-player-info"
                , body = Http.emptyBody
                , expect = Http.expectJson playerDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send App.MyPlayerInfoLoaded request


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
                , body =
                    Http.jsonBody
                        (Json.Encode.string title)
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send (always Game.GameTitleUpdated) request
