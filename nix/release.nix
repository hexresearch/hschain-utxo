{ isProd    ? false
}:
let
  lib = import ./lib.nix;
  
  noCheck = [
      "timeout"
      "servant-client" 
      "haskeline"
      "repline" ];

  cabalProject     = lib.parseCabalProject { path = ./..; };
  haskellOverrides = lib.projectOverrides { inherit cabalProject noCheck; };
  config           = lib.configOverrides haskellOverrides;  
  pkgs             = import ./pkgs.nix { inherit config; overlays = []; };
in
  lib.releaseSet pkgs cabalProject 
