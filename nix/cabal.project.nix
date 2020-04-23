# List of local packages
#
# We do not use this module right now in favor of lib.parseCabalProject
# which finds the cabal submodules automatically.
#
# It's here as example.
let 
  package = name: { 
    name = name;  
    path = ./.. + (/. + name);
  };

  packageAt = path: name: {
    name = name;
    path = ./.. + path + (/. + name);
  };

  packages   =       ps: builtins.map package          ps;
  packagesAt = path: ps: builtins.map (packageAt path) ps;

  project = ps: builtins.concatLists ps;

in project [
    (packages [ 
      "hex-common"
      "hindley-milner-tags"
      "hschain-utxo"
      "hschain-utxo-compiler"
      "hschain-utxo-lang"
      "hschain-utxo-repl"
      "hschain-utxo-service"
      "hschain-utxo-state"
      "hschain-utxo-test"
    ])

    (packagesAt /hschain-utxo-api [
      "hschain-utxo-api-client"
      "hschain-utxo-api-rest"
    ])
  ]
