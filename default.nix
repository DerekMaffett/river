(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    app = ./app;
  };

  shells = {
    ghc = ["app"];
  };
})
