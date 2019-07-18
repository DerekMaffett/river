with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "river";
  buildInputs = [
    stack
  ];
}
