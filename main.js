#!/usr/bin/env node

const Elm = require("./dist/main.js");
const fs = require("fs");
const path = require("path");
const child_process = require("child_process");

var localeCodes = fs.readdirSync("./cldr-misc-full/main/");

var rawData = {};

localeCodes.forEach(function(localeCode) {
  rawData[localeCode] = {
    "delimiters":
      fs.readFileSync("./cldr-misc-full/main/" + localeCode + "/delimiters.json", "utf-8"),
    "listPatterns":
      fs.readFileSync("./cldr-misc-full/main/" + localeCode + "/listPatterns.json", "utf-8"),
    "numbers":
      fs.readFileSync("./cldr-numbers-full/main/" + localeCode + "/numbers.json", "utf-8"),
    "currencies":
      fs.readFileSync("./cldr-numbers-full/main/" + localeCode + "/currencies.json", "utf-8")
  };
});

var worker = Elm.Main.worker({
  "rawData": rawData,
  "cardinals": fs.readFileSync("./cldr-core/supplemental/plurals.json", "utf-8"),
  "ordinals": fs.readFileSync("./cldr-core/supplemental/ordinals.json", "utf-8"),
  "numberingSystems": fs.readFileSync("./cldr-core/supplemental/numberingSystems.json", "utf-8")
});


worker.ports.reportError.subscribe(function(data) {
  var errorMsg = data["error"];

  errorMsg.split("\\n").forEach(function(line) {
    console.log(line);
  });

  process.exit(1);
});


worker.ports.writeModule.subscribe(function(data) {
  var directory = data["directory"];
  var name = data["name"];
  var content = data["content"];

  var modulePath =
    directory
      .concat([ name ])
      .join(path.sep);


  var lastDir = null; 

  directory.forEach(function(dir) {
    if (lastDir === null) {
      lastDir = dir;
    } else {
      lastDir = lastDir + path.sep + dir;
    }

    if (!fs.existsSync(lastDir)) {
      fs.mkdirSync(lastDir);
    }
  });


  fs.writeFileSync(modulePath, content);

  child_process.exec(
    "elm-format --yes " + modulePath,
    function(err, stdout, stderr) {
      if (err != undefined) { return; }
    }
  );

  console.log("Generated: " + modulePath);
});
