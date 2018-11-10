module API
    exposing
    ( register
    , login
    , logout
    , getAllGames
    , newGame
    , createInvite
    , joinGame
    )

import Http
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (encode)
import Lobby.Types as Lobby
import Login.Types as Login
import Game.Types as Game
import Invite
import PouchDB.Decode
    exposing
        ( decodeGameId
        , decodeGameList
        , gameIdDecoder
        , gameListDecoder
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


newGame : String -> Cmd Lobby.Msg
newGame title =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/games"
                , body = Http.jsonBody (Json.Encode.string title)
                , expect = Http.expectJson gameIdDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        RemoteData.sendRequest request
            |> Cmd.map (always Lobby.GetGameList)


login : Login.Auth -> Cmd Login.ConsumerMsg
login auth =
    let
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = domain ++ "/login"
                , body = Http.jsonBody (Login.encodeAuth auth)
                , expect = Http.expectJson Json.Decode.int
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
