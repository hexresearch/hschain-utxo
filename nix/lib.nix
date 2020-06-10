with builtins;
let
  lib = (import <nixpkgs> {}).haskell.lib;
  callInternal = hsPkgs: name: path: args: (
    lib.dontHaddock (hsPkgs.callCabal2nix name path args ));
 
  tryEval = var : default :
    with builtins.tryEval var;
      if success
        then value
        else default;

  readConfig = overrideCfg : cfgFile :
     let inFile = tryEval overrideCfg cfgFile;
     in builtins.fromJSON (builtins.readFile inFile);

  callFromVersions = {
      callCabal2nixWithOptions
    , fetchgitPrivate
    , pkgConfig
    }: repo: name: cabal2NixOpts: args: callCabal2nixWithOptions "${name}" (fetchgitPrivate pkgConfig."${repo}") cabal2NixOpts args;

  pkgConfig = readConfig <cfg> ./versions.json;
  configOverrides = haskOverrides:
    let 
      over    = pack: new: pack.override { overrides = new; };
      setHask = pack: over pack haskOverrides;    
    in {
      allowUnfree = true;
			allowBroken = true;
      packageOverrides = ps: rec {
        haskellPackages = setHask ps.haskellPackages;
        haskell = ps.haskell // {
          packages = ps.haskell.packages // {
            ghc843 = setHask ps.haskell.packages.ghc843;
            ghc865 = setHask ps.haskell.packages.ghc865;
          };
        };
        inherit tryEval pkgConfig;
      };
    };    

  releaseSet = pkgs: cabalProject:
    let pkgNames = (map (x: x.name) cabalProject);
        getPkg   = name: { name = name; value = pkgs.haskell.packages.ghc843.${name}; };
    in  rec {
      inherit pkgs;
      hschainUtxoPackages = {
        # inherit (pkgs.haskell.packages.ghc843);
      } // (
        listToAttrs (map getPkg pkgNames)
      );    
  };

  # Creates overrides for local submodules.
  # It expects a list of sets { name, path }
  localsToOverrides = cabalProject: hsPkgs: 
    let 
      call = name: path: callInternal hsPkgs name path {};
      toPackage = { name, path }: { 
        name  = name;  
        value = call name path;
      };
    in 
      listToAttrs (map toPackage cabalProject);

  projectOverrides = {cabalProject, noCheck ? []}: hsNew: hsOld: 
    let
      # Overrides from cabal2nix files
      derivations = lib.packagesFromDirectory { directory = ./derivations; } hsNew hsOld;
      # Local overrides
      locals = localsToOverrides cabalProject hsNew;
      # Suppress tests
      stopCheck = x: { name = x; value = lib.dontCheck (hsOld // derivations).${x}; };
      noCheckLibs = listToAttrs (map stopCheck noCheck);
    in 
      derivations // locals // noCheckLibs // {
        mkDerivation = args: hsOld.mkDerivation (args // {
          doCheck = false;        
          enableLibraryProfiling = false;
          doHaddock = false;
        });
      };      

  attrsToList = set: builtins.attrValues (
    builtins.mapAttrs (name: value: { name = name; value = value; }) set);

  getDirs = dir: 
        builtins.map (x: x.name) (
          builtins.filter (x: x.value == "directory") (
            attrsToList (
            builtins.readDir dir)));

  # Parses typical cabal project with submodules.
  # It takes the root directory of cabal-project and
  # returns list of all submodules with names and directories, i.e.
  # list of sets of the type { name, path }
  #
  # Assumption: submodule with name foo contains file name foo.cabal.
  # 
  # It can be used to create overrides with function localsToOverrides
  # So if we wanto to create overrides for our project we can do it like this:
  # 
  # projectOverrides (parseCabalProject { path = ./.. }) []
  #
  # aux parameter depth limits the depth of recursive top-down traversal
  # for subdirectories. Equals 3 by default.
  parseCabalProject = {path, depth ? 3 }: 
    let
      cabalFile = name: path + (/. + name) + (/. + (name + ".cabal"));

      isSubmodule = name: builtins.pathExists (cabalFile name);
      
      childrenDirs = getDirs path;
      subModules = builtins.filter isSubmodule childrenDirs;
      candidates = builtins.filter (x: !(isSubmodule x)) childrenDirs;
      next = builtins.concatLists (
                builtins.map (x: parseCabalProject { path = path + (/. + x); depth = depth - 1; }) candidates);

      toProject = name: { name = name; path = path + (/. + name); };
    in
      if (depth < 0) 
        then []
        else builtins.map toProject subModules ++ next;

in
  { inherit 
      localsToOverrides
      projectOverrides
      parseCabalProject
      attrsToList
      getDirs 
      configOverrides
      releaseSet
      tryEval
      readConfig
      callFromVersions;
  }

