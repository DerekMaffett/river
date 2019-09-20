{ mkDerivation, aeson, aeson-pretty, base, bytestring, containers
, data-default-class, directory, graphql, haskeline, hpack
, hslogger, hspec, mtl, optparse-applicative, parsec, process
, QuickCheck, req, safe, safe-exceptions, split, stdenv, text
, typed-process, unordered-containers, vector
}:
mkDerivation {
  pname = "river";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers data-default-class
    directory graphql haskeline hslogger mtl optparse-applicative
    parsec process req safe safe-exceptions split text typed-process
    unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring containers data-default-class
    directory graphql haskeline hslogger hspec mtl optparse-applicative
    parsec process QuickCheck req safe safe-exceptions split text
    typed-process unordered-containers vector
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/DerekMaffett/river#readme";
  license = stdenv.lib.licenses.mit;
}
