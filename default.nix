(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    backend = ./backend;
  };

  shells = {
    ghc = ["backend"];
  };
})
