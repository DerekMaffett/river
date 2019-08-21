let
  pkgs = import <nixpkgs> {};
  app = import ./default.nix;
in
pkgs.mkShell {
  name="river";
  inputsFrom=[
    app.shells.ghc
  ];
  buildInputs=[
    pkgs.fswatch
    (pkgs.writeScriptBin "watch-dev" "fswatch -r -o -l 0.2 ./app/ | (while read; do cabal new-build all; done)")
  ];
}
