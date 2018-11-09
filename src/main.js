'use strict';

var app = Elm.Main.init();

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
        } else if (resp.status >= 400 && resp.status < 600) {
          console.log("Game Load Failed");
          app.ports.gameLoadFailed.send(0);
          remote.close();
        } else {
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
      getGame();
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

  // Load game document from local database
  function getGame () {
    local.get("game").then(function (doc) {
      console.log("Game Loaded", { id: id, ref: local, game: doc });
      app.ports.gameLoaded.send({
        id: id,
        ref: local,
        game: doc
      });
    }).catch(function (err) {
      console.log("GetGame Error", err);
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
