{ isProd    ? false
}:
let
  # Packages for 
  pkgs = import ./pkgs.nix { inherit config; overlays = []; };
  # Versions of external libs (hschain)
  pkgConfig = readConfig <cfg> ./versions.json;
  # ---
  lib     = pkgs.haskell.lib;
  tryEval = var : default :
    with builtins.tryEval var;
      if success
        then value
        else default;
  readConfig = overrideCfg : cfgFile :
     let inFile = tryEval overrideCfg cfgFile;
     in builtins.fromJSON (builtins.readFile inFile);

  # Project config
  config = haskOverrides:
    let
      over    = pack: new: pack.override { overrides = new; };
      setHask = pack: over pack haskellOverrides;
    in {
      allowUnfree = true;
			allowBroken = true;
      packageOverrides = ps: rec {
        haskellPackages = setHask ps.haskellPackages;
        haskell = ps.haskell // {
          packages = ps.haskell.packages // {
            ghc843 = setHask ps.haskell.packages.ghc843;
            ghc865 = setHask ps.haskell.packages.ghc865;
            ghc883 = setHask ps.haskell.packages.ghc883;
          };
        };
        inherit tryEval pkgConfig;
      };
    };

  # 
  localProjects =
    let 
      callInternal = name: path: args: hsPkgs: (
        lib.dontHaddock (hsPkgs.callCabal2nix name path args ));
    in {
      "hex-common"              = callInternal "hex-common"              ../hex-common              {};
      "hindley-milner-tags"     = callInternal "hindley-milner-tags"     ../hindley-milner-tags     {};
      "hschain-utxo-blockchain" = callInternal "hschain-utxo-blockchain" ../hschain-utxo-blockchain {};
      "hschain-utxo-state"      = callInternal "hschain-utxo-state"      ../hschain-utxo-state      {};
      "hschain-utxo-compiler"   = callInternal "hschain-utxo-compiler"   ../hschain-utxo-compiler   {};
      "hschain-utxo-repl"       = callInternal "hschain-utxo-repl"       ../hschain-utxo-repl       {};
      "hschain-utxo-lang"       = callInternal "hschain-utxo-lang"       ../hschain-utxo-lang       {};
      # PBFT node
      "hschain-utxo-api-rest"   = callInternal "hschain-utxo-api-rest"   ../hschain-utxo-api/hschain-utxo-api-rest {};
      "hschain-utxo-api-client" = callInternal "hschain-utxo-api-client" ../hschain-utxo-api/hschain-utxo-api-client {};
      "hschain-utxo-service"    = callInternal "hschain-utxo-service"    ../hschain-utxo-service    {};
      "hschain-utxo-test"       = callInternal "hschain-utxo-test"       ../hschain-utxo-test       {};
      "hschain-utxo"            = callInternal "hschain-utxo"            ../hschain-utxo            {};
      # PoW node
      "hschain-utxo-pow-node"   = callInternal "hschain-utxo-pow-node"   ../hschain-utxo-pow-node   {};
      "hschain-pow-func"        = callInternal "hschain-pow-func"        ../hschain-pow-func        {};
      "hschain-pow-check"       = callInternal "hschain-pow-check"       ../hschain-pow-check       {};
    };
  haskellOverrides = hsNew: hsOld:
    let
      # Overrides from cabal2nix files
      derivations = lib.packagesFromDirectory { directory = ./derivations; } hsNew hsOld;
      # Local overrides
      # HSChain packages
      callHSChain = name: hsNew.callCabal2nixWithOptions name
        (builtins.fetchGit pkgConfig.hschain)
        ("--subpath " + name)
        {};
      callHSChainWithDir = name: dir: hsNew.callCabal2nixWithOptions name
        (builtins.fetchGit pkgConfig.hschain)
        ("--subpath " + dir)
        {};
    in
      derivations // (builtins.mapAttrs (_: f: f hsNew) localProjects) // {
        mkDerivation = args: hsOld.mkDerivation (args // {
          # enableLibraryProfiling = false;
          doHaddock = false;
        });
      } // {
        # HSChain dependencies
        hschain-control  = callHSChain "hschain-control";
        hschain-config   = callHSChain "hschain-config";
        hschain-mempool  = callHSChain "hschain-mempool";
        hschain-merkle   = callHSChain "hschain-merkle";
        hschain-logger   = callHSChain "hschain-logger";
        hschain-crypto   = callHSChain "hschain-crypto";
        hschain-types    = callHSChain "hschain-types";
        hschain-net      = callHSChain "hschain-net";
        hschain-db       = callHSChain "hschain-db";
        hschain-PoW      = callHSChain "hschain-PoW";
        hschain          = callHSChain "hschain";
        # Disable tests
        timeout         = lib.dontCheck hsOld.timeout;
        repline         = lib.dontCheck hsOld.repline;
      };
in
let pkgNames = builtins.attrNames localProjects;
    getPkg   = name: { name = name; value = pkgs.haskell.packages.ghc883.${name}; };
in  rec {
  inherit pkgs;
    hschainUtxoPackages = {
        # inherit (pkgs.haskell.packages.ghc843);
    } // (
      builtins.listToAttrs (map getPkg pkgNames)
    )
    ;
  }
