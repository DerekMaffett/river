#!/usr/bin/env node

const fs = require("fs");
const os = require("os");

const platform = os.platform();

if (platform === "linux") {
    fs.renameSync("./bin/s3/linux/river", "./bin/river");
} else if (platform === "darwin") {
    fs.renameSync("./bin/s3/osx/river", "./bin/river");
} else {
    console.log(
        "OS not recognized or explicitly supported. Installing Linux binaries..."
    );
    fs.renameSync("./bin/s3/linux/river", "./bin/river");
}
