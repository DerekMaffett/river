let
  pkgs = import <nixpkgs> {};
  app = import ./reflex.nix;
in
pkgs.mkShell {
  name="river";
  inputsFrom=[
    app.shells.ghc
  ];
  buildInputs=[
    (pkgs.writeWatchScript {
      name = "watch-dev";
      src = "./app";
      command = "cabal new-build all";
    })
    (pkgs.localCabalRun "river")
  ];
}
