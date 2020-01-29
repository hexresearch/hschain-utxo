args@{ isProd ? false
}:
let
  release = import ./release.nix args;
in
  with release.pkgs.haskellPackages; release.pkgs.buildEnv {
    name  = "hschain-utxo";
    paths = release.pkgs.lib.attrValues release.hschainUtxoPackages;
  }

