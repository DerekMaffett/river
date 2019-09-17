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
    pkgs.haskellPackages.hpack
    (pkgs.writeWatchScript {
      name = "watch-dev";
      src = "./app";
      command = "cabal new-build all";
    })
    (pkgs.localCabalRun "river" "exe:river")
    (pkgs.writeShellScriptBin "hpack-river" "cd ./app && hpack && cd ..")
  ];
}
