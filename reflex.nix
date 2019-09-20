{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    app = ./.;
  };

  shells = {
    ghc = ["app"];
  };
})
