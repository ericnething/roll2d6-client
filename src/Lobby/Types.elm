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

module Lobby.Types exposing (..)

import Game.Types as Game
import Game.GameType as Game
import Game.Player exposing (Player)
import RemoteData exposing (RemoteData(..), WebData)
import Http


type alias Model r =
    { r |
      games : WebData (List Game.GameSummary)
    , newGameForm : NewGameForm
    , lobbyTab : Tab
    }

type Tab
    = GamesTab
    | InvitesTab
    | MessagesTab
    | SettingsTab SettingsTab

type SettingsTab
    = AccountTab AccountModel
    | AppearanceTab

type AccountModel
    = ShowAccount
    | EditAccount EditAccountForm
    | ChangePassword ChangePasswordForm

defaultAccountModel = ShowAccount

type alias EditAccountForm =
    { displayName : String
    , username : String
    , email : String
    , password : String
    }

type alias ChangePasswordForm =
    { displayName : String
    , username : String
    , email : String
    , newPassword : String
    , currentPassword : String
    }

type NewGameForm
    = NewGameFormNone
    | NewGameForm NewGameFormModel

type alias NewGameFormModel =
    { title : String
    , gameType : Game.GameType
    }

type Msg
    = NewGame
    | NewGameResponse (WebData Game.GameId)
    | UpdateNewGameTitle String
    | UpdateNewGameType Game.GameType
    | GetGameList
    | SetGameList (WebData (List Game.GameSummary))
    | LoadGame Game.GameId
    | Logout
    | LogoutResponse (Result Http.Error String)
    | OpenNewGameForm
    | CloseNewGameForm
    | SwitchTab Tab
    | ResumeGame
    | SwitchSettingsTab SettingsTab
    | NoOp
    | UpdateAccountDisplayName String
    | UpdateAccountEmail String
    | UpdateAccountCurrentPassword String
    | UpdatePasswordNewPassword String
    | UpdatePasswordCurrentPassword String
