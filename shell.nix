let
  pkgs = import <nixpkgs> {};
  app = import ./default.nix;
in
pkgs.mkShell {
  name="river";
  inputsFrom=[
    app.env
  ];
  buildInputs=[
    pkgs.haskellPackages.hpack
    (pkgs.writeWatchScript {
      name = "watch-dev";
      src = "./app";
      command = "cabal new-build exe:river --ghc-options=-dynamic";
    })
    (pkgs.writeShellScriptBin "river" "cabal new-run exe:river $@ --ghc-options=-dynamic")
    (pkgs.writeShellScriptBin "river-gui" "cabal new-run river-gui $@ --ghc-options=-dynamic")
  ];
}
