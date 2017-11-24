#!/use/bin/env node

"use strict";

const Elm = require("./dist/elm.js");
const fs = require("fs-extra");
const path = require("path");
const child_process = require("child_process");

const currentDir = process.cwd();

let cardinals =
  fs
    .readFileSync(
      path.join(
        currentDir,
        "cldr-core",
        "supplemental",
        "plurals.json",
      )
    )
    .toString();

let ordinals =
  fs
    .readFileSync(
      path.join(
        currentDir,
        "cldr-core",
        "supplemental",
        "ordinals.json",
      )
    )
    .toString();


let worker = Elm.Generator.worker({
  "cardinalsJson": cardinals,
  "ordinalsJson": ordinals,
});


worker.ports.exportResult.subscribe(function(content) {
  let outputDir = path.join(
    currentDir,
    "src",
    "Localized"
  );

  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir);
  }

  content.modules.forEach(function(module) {
    let moduleFilePath =
      path.join(
        currentDir,
        "src",
        "Localized",
        module.filename
      );

    console.log("Generate Localized." + module.filename);

    fs.writeFileSync(
      moduleFilePath,
      module.content
    );

    child_process.exec(
      "elm-format --yes " + moduleFilePath, function(err, stdout, stderr) {
        if (err != undefined) {
          return;
        }
      }
    );
  });

  console.log("waiting for elm-format to finish");
});
