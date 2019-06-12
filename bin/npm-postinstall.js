#!/usr/bin/env node

const fs = require("fs");
const os = require("os");

const platform = os.platform();

if (platform === "linux") {
    fs.renameSync("./bin/river-linux", "./bin/river");
} else if (platform === "darwin") {
    fs.renameSync("./bin/river-osx", "./bin/river");
} else {
    console.log(
        "OS not recognized or explicitly supported. Installing Linux binaries..."
    );
    fs.renameSync("./bin/river-linux", "./bin/river");
}
