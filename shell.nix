let
  pkgs = import <nixpkgs> {};
  app = import ./reflex.nix {};
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
      command = "cabal new-build exe:river --ghc-options=-dynamic";
    })
    (pkgs.writeShellScriptBin "river" "cabal new-run exe:river $@ --ghc-options=-dynamic")
    (pkgs.writeShellScriptBin "hpack-river" "cd ./app && hpack && cd ..")
  ];
}
