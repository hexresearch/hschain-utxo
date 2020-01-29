let
  lib  = import ./lib.nix;
  noCheck = [
      "timeout"
      "servant-client" 
      "haskeline"
      "repline" ];
in
  rec { 
    haskellOverrides = lib.projectOverrides { inherit cabalProject noCheck; };
    cabalProject     = lib.parseCabalProject { path = ./..; };
  }
    
