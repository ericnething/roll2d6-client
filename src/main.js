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

const app = Elm.Main.init({
  flags: [ document.documentElement.clientWidth,
           document.documentElement.clientHeight
         ]
});

//----------------------------------------------
//  Couch DB
//----------------------------------------------

// New database connection
app.ports.loadGame.subscribe(function (id) {

  const uuid_re = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

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
            if (uuid_re.test(row.id)) {
              data.sheets[row.id] = row.doc;
            } else if (row.id === "game") {
              data.game = row.doc;
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
        
        // setup event source
        const eventSource = setupEventSource(id);
        console.log("Event Source: ", eventSource);

        app.ports.gameLoaded.send({
          id: id,
          ref: local,
          game: game.game,
          sheets: game.sheets,
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
      const updatedDocs = (function() {
          let data = {
            game: null,
            sheets: {}
          }
          change.docs.forEach(function(doc) {
            if (uuid_re.test(doc._id)) {
              data.sheets[doc._id] = doc;
            } else if (doc._id === "game") {
              data.game = doc;
            }
          })
          return data;
      })();
      console.log("Updated Docs", updatedDocs);
      app.ports.changesReceived.send(updatedDocs);

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

app.ports.closeEventStream.subscribe(function (eventSource) {
  console.log("Close EventStream");
  eventSource.close();
});


//----------------------------------------------
//  Drag and Drop
//----------------------------------------------

app.ports.dragstart.subscribe(function (event) {
  event.dataTransfer.setData('text', '');
});
