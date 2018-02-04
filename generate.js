"use strict";

const Elm = require("./dist/elm.js");
const fs = require("fs-extra");
const path = require("path");
const child_process = require("child_process");

const currentDir = process.cwd();

let numberFormats = []

fs
  .readdirSync(
    path.join(
      currentDir,
      "cldr-numbers-full",
      "main"
    )
  )
  .forEach(function(localeCode) {
    numberFormats.push([ 
      localeCode,
      fs
        .readFileSync(
          path.join(
            currentDir,
            "cldr-numbers-full",
            "main",
            localeCode,
            "numbers.json"
          )
        )
        .toString()
    ]);
  });

let cardinals =
  fs
    .readFileSync(
      path.join(
        currentDir,
        "cldr-core",
        "supplemental",
        "plurals.json"
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
        "ordinals.json"
      )
    )
    .toString();


let worker = Elm.Generator.worker({
  "numberFormatsJsons": numberFormats,
  "cardinalsJson": cardinals,
  "ordinalsJson": ordinals,
});


worker.ports.exportResult.subscribe(function(content) {
  let outputDir = path.join(
    currentDir,
    "generated",
    "Translation"
  );

  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir);
  }

  content.modules.forEach(function(module) {
    let moduleFilePath =
      path.join(
        currentDir,
        "generated",
        "Translation",
        module.filename
      );

    console.log("Generate Translation." + module.filename);

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
