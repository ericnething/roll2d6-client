'use strict';

var app = Elm.Main.fullscreen();

var db = new PouchDB("test_db");

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
