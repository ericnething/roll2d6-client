/*

Roll2d6 Virtual Tabletop Project

Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>

This program is free software: you can redistribute it and/or
modify it under the terms of the GNU Affero General Public License
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program. If not, see
<https://www.gnu.org/licenses/>.

*/

"use strict";

const app = Elm.Main.init({
  flags: {
    windowSize: [
      document.documentElement.clientWidth,
      document.documentElement.clientHeight
    ],
    xmppClient: createChatClient()
  }
});

//----------------------------------------------
//  Couch DB
//----------------------------------------------

// New database connection
app.ports.loadGame.subscribe(function (id) {

  const uuid_re = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

  const gameId = "game_" + id;
  const domain = window.location.origin + "/api/couchdb/";
  const remoteUrl = domain + gameId;
  const local = new PouchDB(gameId);
  const remote = new PouchDB(remoteUrl, {
    skip_setup: true,
    fetch: function (url, opts) {
      return PouchDB.fetch(url, opts).then(function (resp) {
        // console.log("Fetch Response: ", resp);
        if (resp.status === 401) {
          console.log("Auth Failed");
          app.ports.authFailed.send(0);
          remote.close();
        } else if (resp.status >= 400 && resp.status < 600 && resp.status !== 404) {
          console.log("Game Load Failed");
          app.ports.gameLoadFailed.send(0);
          remote.close();
        } else {
          // console.log(resp);
          return resp
        }
      })
    }
  });

  // Perform a one-time one-way replication from remote to local
  local.replicate.from(remote).on("complete", function () {
    console.log("Replication Successful")

    // Check if the game document exists
    local.get("game").then(function (doc) {
      // It exists.
    }).catch(function (err) {
      console.log("Error: ", err);
    }).finally(function () {
      // Set up syncing
      sync();
      // Send the game document back to Elm
      local.allDocs({
        include_docs: true
      }).then(function (result) {
        console.log(result);

        const game = (function() {
          let data = {
            game: null,
            sheets: {}
          }
          result.rows.forEach(function(row) {
            if (row.id === "game")
            {
              data.game = row.doc;
            }
            else
            {
              data.sheets[row.id] = row.doc;
            }
          })
          return data;
        })();

        console.log(
          "Game Loaded",
          { id: id,
            ref: local,
            game: game.game,
            sheets: game.sheets
          });

        app.ports.gameLoaded.send({
          id: id,
          ref: local,
          game: game.game,
          sheets: game.sheets
        });
        
      }).catch(function (err) {
        console.log("GetGame Error", err);
      });
    });
  }).on("error", function (err) {
    console.log("Replication Error: ", err);
  });

  // Sync local and remote databases
  function sync () {
    // local.sync(remote, {
    local.replicate.from(remote, {
      live: true,
      retry: true
    }).on("complete", function () {
      console.log("Sync Successful");

    }).on("change", function (change) {
      // yo, something changed!
      console.log("Changes Received", change);
      const updatedDocs = (function() {
        let data = {
          game: null,
          sheets: {}
        }
        change.docs.forEach(function(doc) {
          if (doc._id === "game")
          {
            data.game = doc;
          }
          else
          {
            data.sheets[doc._id] = doc;
          }
        })
        return data;
      })();
      console.log("Updated Docs", updatedDocs);
      app.ports.changesReceived.send(updatedDocs);

    }).on("paused", function (info) {
      // replication was paused, usually because of a lost connection
      console.log("Paused", info);
      
    }).on("active", function (info) {
      // replication was resumed
      console.log("Resumed", info);

    }).on("error", function (err) {
      console.log("Sync Error", err);
    });

    local.replicate.to(remote, {
      live: true,
      retry: true
    }).on("complete", function () {
      console.log("Sync Successful");

    }).on("change", function (change) {
      // yo, something changed!
      console.log("Changes sent", change);

    }).on("paused", function (info) {
      // replication was paused, usually because of a lost connection
      console.log("Paused", info);
      
    }).on("active", function (info) {
      // replication was resumed
      console.log("Resumed", info);

    }).on("error", function (err) {
      console.log("Sync Error", err);
    });
  }

});

// Write to database
app.ports.put.subscribe(function (args) {
  const db = args[0];
  const docId = args[1];
  const newDoc = args[2];
  db.get(docId).then(function (doc) {
    return db.put(
      Object.assign(newDoc, { _id: docId, _rev: doc._rev })
    );
  }).then(function (response) {
    // handle response
    console.log(response)
  }).catch(function (err) {
    console.log("Error: ", err);
    if (err.status && err.status === 404) {
        // The document doesn't exist yet, so let's create it.
      db.put(
        Object.assign(newDoc, { _id: docId })
      );
    }
  });
});

// Delete document from database
app.ports.remove.subscribe(function (args) {
  const db = args[0];
  const docId = args[1];
  db.get(docId).then(function (doc) {
    return db.remove(doc);
  }).then(function (response) {
    // handle response
    console.log(response)
  }).catch(function (err) {
    console.log("Error: ", err);
  });
});


//----------------------------------------------
//  Drag and Drop
//----------------------------------------------

app.ports.dragstart.subscribe(function (event) {
  event.dataTransfer.setData("text", "");
});


//----------------------------------------------
//  XMPP
//----------------------------------------------

function createChatClient() {
  const client = XMPP.createClient({
    jid: "welkin@localhost" , //data.jid,
    password: "foobar", //data.password,
    wsURL: "ws://localhost:5280/ws-xmpp",
    // boshURL: "http://localhost:5280/bind-http",
    transports: ["websocket"],
    useStreamManagement: true,
    resource: "web"
  });

  client.enableKeepAlive({ interval: 45, timeout: 120 });

  client.on("connected", function() {
    console.log("connected at " + new Date);
    app.ports.xmpp_connected.send(null);
  });

  client.on("disconnected", function() {
    console.log("disconnection at " + new Date);
    client.connect();
  });

  client.on("message", function(chat) {
    // console.log("chat received: ", chat);
    chat.stanzaType = "message";
    chat.timestamp = chat.delay && Date.now(chat.delay.stamp) || Date.now();
    app.ports.xmpp_received.send(chat);
  });

  client.on("presence", function(presence) {
    // console.log("presence received: ", presence);
    presence.stanzaType = "presence";
    app.ports.xmpp_received.send(presence);
  });

  client.on("raw:incoming", function (name, data) {
    console.log(name, data);
  });

  client.on("raw:outgoing", function (name, data) {
    console.log(name, data);
  });

  client.on("session:started", function() {
    client.enableCarbons(function(err) {
      if (err) {
        console.log("Server does not support carbons");
      }
    });

    client.getRoster(function(err, resp) {
      client.updateCaps();
      client.sendPresence({
        caps: client.disco.caps
      });
    });
  });
  return client;
}

const xmppCommand = {
  connect: function(client) {
    client.connect();
  },
  disconnect: function(client) {
    client.disconnect();
  },
  setRoomAffiliation: function(client, { room, person, affiliation }) {
    client.setRoomAffiliation(room, person, affiliation);
  },
  createGameRoom: function(client, { room, title, gameType, nickname }) {
    client.joinRoom(room, nickname);
    client.configureRoom(room, {
      fields: [
        { name: "FORM_TYPE",
          value: "http://jabber.org/protocol/muc#roomconfig"
        },
        { name: "muc#roomconfig_persistentroom", value: "1" },
        { name: "muc#roomconfig_membersonly", value: "1" },
        { name: "muc#roomconfig_roomname", value: title },
        { name: "muc#roomconfig_roomdesc", value: gameType }
      ]
    });
  },
  addPlayer: function(client, { room, person }) {
    client.setRoomAffiliation(room, person, "member");
  },
  removePlayer: function(client, { room, person }) {
    client.setRoomAffiliation(room, person, "none");
  },
  joinRoom: function (client, { displayName, room }) {
    client.joinRoom(`${room}@muc.localhost`, displayName, {
      status: "It's going to be a great day",
      joinMuc: {
        history: {
          maxstanzas: 20
        }
      }
    });
  },
  leaveRoom: function (client, { displayName, room }) {
    client.leaveRoom(`${room}@muc.localhost`, displayName);
  },
  sendMessage: function (client, message) {
    message.type = "groupchat";
    // message.to = "test@muc.localhost";
    client.sendMessage(message);
  }
}

app.ports.xmpp_send.subscribe(function (args) {
  const client = args.client;
  const command = args.command;
  const data = args.data;
  const cmd = xmppCommand[command];
  if (cmd) {
    cmd(client, data);
  }
});
