args@{
  isProd      ? false
}:
let release = import ./release.nix args;
    lib = (import <nixpkgs> {}).lib;
    pkgs = release.pkgs;
in release.pkgs.haskellPackages.shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
    ];
    buildInputs = with pkgs; [
      #rabbitmq_server
    ];
    packages = _: 
      let packs = pkgs.lib.attrValues release.hschainUtxoPackages;
          names = pkgs.lib.attrNames  release.hschainUtxoPackages;
      in  lib.traceSeq names packs ;
    # NOTE: this is workaround for problem with building
    #       hschain-types. Without this build fails
    #       mysteriously. Note that build with nix-build is not
    #       affected.
    shellHook = "";
#    shellHook = ''
#      export LD_LIBRARY_PATH=${pkgs.libsodium}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
#      '';
  }
