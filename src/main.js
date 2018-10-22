'use strict';

var app = Elm.Main.fullscreen();

var db = new PouchDB("test_db");

var remoteDB = new PouchDB('http://localhost:5984/test_db');

// db.replicate.to(remoteDB).on("complete", function () {
//   console.log("Replication Successful")
// }).on("error", function (err) {
//   console.log("Replication Error: ", err)
// });

db.sync(remoteDB, {
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

app.ports.put.subscribe(function (newDoc) {
  db.get(newDoc._id).then(function (doc) {
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

app.ports.get.subscribe(function (id) {
  db.get(id).then(function (doc) {
    console.log(doc);
    app.ports.getResponse.send(doc);
  }).catch(function (err) {
    console.log(err);
  });
});

app.ports.allDocs.subscribe(function () {
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
