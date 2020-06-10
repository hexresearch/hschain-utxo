args@{
  isProd      ? false
}:
let release = import ./release.nix args;
    pkgs = release.pkgs;
in release.pkgs.haskellPackages.shellFor {
    nativeBuildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
    ];
    buildInputs = with pkgs; [
      #rabbitmq_server
    ];
    packages = _: pkgs.lib.attrValues release.hschainUtxoPackages;
    shellHook = ''
      '';
  }
