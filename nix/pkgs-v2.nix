# Release is pinned to the stable 1909 branch
let
  rev     = "1dadefec574531f021bddc85ca3c9bc81fd52899";
  sha256  = "sha256:0csm7nycb8p990jl1mj20mg79lxpfg2xcplndcg1dbqclgw1wmqd";
in
import (builtins.fetchTarball {
  inherit sha256;
  name   = "nixos-1909";
  url    = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
})
