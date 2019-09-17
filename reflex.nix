{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    app = ./app;
  };

  shells = {
    ghc = ["app"];
  };
})
