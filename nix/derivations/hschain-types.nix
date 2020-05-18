{ mkDerivation, aeson, async, base, base58-bytestring, bytestring
, containers, data-default-class, deepseq, hschain-crypto
, QuickCheck, random, serialise, sqlite-simple, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, time, unordered-containers
, vector, vector-th-unbox
, fetchgit, tryEval, pkgConfig
}:
mkDerivation {
  pname = "hschain-types";
  version = "0.1";
  src = tryEval <hschain> (fetchgit pkgConfig.hschain);
  postUnpack = "sourceRoot+=/hschain-types; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson async base base58-bytestring bytestring containers
    data-default-class deepseq hschain-crypto serialise sqlite-simple
    text time unordered-containers vector vector-th-unbox
  ];
  doCheck = false;
#  testHaskellDepends = [
#    base bytestring hschain-crypto QuickCheck random serialise tasty
#    tasty-hunit tasty-quickcheck
#  ];
  homepage = "https://github.com/hexresearch/hschain";
  description = "Core data types of HSChain";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
