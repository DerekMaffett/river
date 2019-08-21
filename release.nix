let
  pkgs = import <nixpkgs> {};
in
  pkgs.buildEnv {
    name="binJuggling";
    paths=[
      pkgs.awscli
    ];
  }
