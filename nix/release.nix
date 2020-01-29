{ isProd    ? false
}:
let
  over    = pack: new: pack.override { overrides = new; };
  setHask = pack: over pack haskOverrides;
  pkgs    = import ./pkgs.nix { inherit config; overlays = []; };

  config = {
    allowUnfree = true;
    packageOverrides = ps: rec {
      haskellPackages = setHask ps.haskellPackages;
      haskell = ps.haskell // {
        packages = ps.haskell.packages // {
          ghc843 = setHask ps.haskell.packages.ghc843;
        };
      };
    };
  };    

  haskOverrides = import ./overrides.nix;
  
  self = rec {
    inherit pkgs;
    hschainUtxoPackages = {
      inherit (pkgs.haskell.packages.ghc843)                  
      hex-common
      hs-check
      hschain-utxo
      hschain-utxo-api-client
      hschain-utxo-api-rest
      hschain-utxo-compiler
      hschain-utxo-lang
      hschain-utxo-repl
      hschain-utxo-service
      hschain-utxo-state
      hschain-utxo-test;
    };
  };
in
  self 
