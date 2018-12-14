// Roll2d6 Virtual Tabletop Project

// Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>

// This program is free software: you can redistribute it
// and/or modify it under the terms of the GNU Affero
// General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your
// option) any later version.

// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the
// implied warranty of MERCHANTABILITY or FITNESS FOR A
// PARTICULAR PURPOSE.  See the GNU Affero General Public
// License for more details.

// You should have received a copy of the GNU Affero General
// Public License along with this program. If not, see
// <https://www.gnu.org/licenses/>.

'use strict';

var app = Elm.Main.init({
  flags: [ document.documentElement.clientWidth,
           document.documentElement.clientHeight
         ]
});

//----------------------------------------------
//  Couch DB
//----------------------------------------------

// New database connection
app.ports.loadGame.subscribe(function (args) {
  const emptyGame = args[0];
  const id = args[1];

  const domain = window.location.origin + "/api/couchdb/";
  const remoteUrl = domain + id;
  const local = new PouchDB(id);
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
          return resp
        }
      })
    }
  });

  function setupEventSource(gameId) {
    const eventSource = new EventSource("/api/subscribe/" + gameId);
    eventSource.onerror = function (ev) {
      console.log("SSE Error", ev);
    };
    eventSource.addEventListener("player-list", function (ev) {
      console.log("Player list", ev.data);
      app.ports.sse_playerListUpdated.send(JSON.parse(ev.data));
    });
    eventSource.addEventListener("player-presence", function (ev) {
      console.log("Player presence", ev.data);
      app.ports.sse_playerPresenceUpdated.send(JSON.parse(ev.data));
    });
    eventSource.addEventListener("chat-message", function (ev) {
      console.log("Chat message", ev.data);
      app.ports.sse_chatMessageReceived.send(JSON.parse(ev.data));
    });
    return eventSource;
  }

  // Perform a one-time one-way replication from remote to local
  local.replicate.from(remote).on("complete", function () {
    console.log("Replication Successful")

    // Check if the game document exists
    local.get("game").then(function (doc) {
      // It exists.
    }).catch(function (err) {
      console.log("Error: ", err);
      if (err.status && err.status === 404) {
        // It doesn't exist yet, so create it.
        local.put(
          Object.assign(emptyGame, { _id: "game" })
        );
      }
    }).finally(function () {
      // Set up syncing
      sync();
      // Send the game document back to Elm
      local.get("game").then(function (doc) {
        console.log("Game Loaded", { id: id, ref: local, game: doc });
        
        // setup event source
        const eventSource = setupEventSource(id);
        console.log("Event Source: ", eventSource);

        app.ports.gameLoaded.send({
          id: id,
          ref: local,
          game: doc,
          eventSource: eventSource
        });

        addBeforeUnloadListener(eventSource, id);
        
      }).catch(function (err) {
        console.log("GetGame Error", err);
      });
    });
  }).on("error", function (err) {
    console.log("Replication Error: ", err);
  });
  
  function addBeforeUnloadListener (eventSource, gameId) {
    window.addEventListener("unload", function (event) {
      // Cancel the event as stated by the standard.
      event.preventDefault();
      // Chrome requires returnValue to be set.
      event.returnValue = '';

      const url = window.location.origin
              + "/api/games/" + gameId
              + "/presence/offline"
      navigator.sendBeacon(url);
      eventSource.close()
    });
  }

  // Sync local and remote databases
  function sync () {
    // local.sync(remote, {
    local.replicate.from(remote, {
      live: true,
      retry: true
    }).on("complete", function () {
      console.log("Sync Successful");

    }).on('change', function (change) {
      // yo, something changed!
      console.log("Changes Received", change);
      app.ports.changesReceived.send();

    }).on('paused', function (info) {
      // replication was paused, usually because of a lost connection
      console.log("Paused", info);
      
    }).on('active', function (info) {
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

    }).on('change', function (change) {
      // yo, something changed!
      console.log("Changes sent", change);

    }).on('paused', function (info) {
      // replication was paused, usually because of a lost connection
      console.log("Paused", info);
      
    }).on('active', function (info) {
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
  const newDoc = args[1];
  db.get("game").then(function (doc) {
    return db.put(
      Object.assign(newDoc, { _rev: doc._rev })
    );
  }).then(function (response) {
    // handle response
    console.log(response)
  }).catch(function (err) {
    console.log("Error: ", err);
    if (err.status && err.status === 404) {
        // The document doesn't exist yet, so let's create it.
      db.put(newDoc);
    }
  });
});

// Read from database
app.ports.get.subscribe(function (db) {
  db.get("game").then(function (doc) {
    console.log(doc);
    app.ports.getResponse.send(doc);
  }).catch(function (err) {
    console.log(err);
  });
});

// Read all documents from database
// app.ports.allDocs.subscribe(function (db) {
//   db.allDocs({ include_docs: true }).then(function (docs) {
//     console.log("Docs: ", docs)
//     return docs.rows.map(
//       object => {
//         const { _id, title } = object.doc;
//         return { _id, title };
//       }
//     )
//   }).then(function (gameList) {
//     console.log("GameMetadataList: ", gameList)
//     app.ports.getGameListResponse.send(gameList);
//   });
// });

app.ports.closeEventStream.subscribe(function (eventSource) {
  console.log("Close EventStream");
  eventSource.close();
});
