let 
  project = import ./reflex.nix;
in
  project.ghc.app
