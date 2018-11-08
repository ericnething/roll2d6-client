'use strict';

var app = Elm.Main.fullscreen();

//----------------------------------------------
//  Couch DB
//----------------------------------------------

// New database connection
app.ports.loadGame.subscribe(function (args) {
  const emptyGame = args[0];
  const id = args[1];
  // const id = "game_e95bcd76-c9fd-4bd4-ba1d-d1cd35760706"

  // const domain = "http://localhost:5984/"
  const domain = window.location.origin + "/api/couchdb/";
  console.log(emptyGame, id);
  const remoteUrl = domain + id;
  console.log("Remote URL", remoteUrl);
  const local = new PouchDB(id);
  console.log("local connected");
  const remote = new PouchDB(remoteUrl); //, { skip_setup: true });
  // const remote = new PouchDB("http://localhost:8080/api/couchdb/game_e95bcd76-c9fd-4bd4-ba1d-d1cd35760706");
  remote.info().then( info => console.log("Remote", info));
  console.log("remote connected!!!!!");
  
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
    console.log("Replication Error: ", err)
    // app.ports.loadGameFailed.send();
  });
  
  // Sync local and remote databases
  function sync () {
    local.sync(remote, {
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
app.ports.allDocs.subscribe(function (db) {
  db.allDocs({ include_docs: true }).then(function (docs) {
    console.log("Docs: ", docs)
    return docs.rows.map(
      object => {
        const { _id, title } = object.doc;
        return { _id, title };
      }
    )
  }).then(function (gameList) {
    console.log("GameMetadataList: ", gameList)
    app.ports.getGameListResponse.send(gameList);
  });
});
