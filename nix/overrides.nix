let
  lib  = import ./lib.nix;
  noCheck = [
      "timeout"
      "servant-client" 
      "haskeline"
      "repline" ];
  cabalProject = lib.parseCabalProject { path = ./..; };
in
  lib.projectOverrides { inherit cabalProject noCheck; }
