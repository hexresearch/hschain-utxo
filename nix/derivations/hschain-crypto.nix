{ mkDerivation, aeson, base, base16-bytestring, base58-bytestring
, base64-bytestring, bytestring, containers, criterion, cryptonite
, deepseq, entropy, Lazy-Pbkdf2, memory, primitive, random
, serialise, SHA, stdenv, tasty, tasty-hunit, text, vector
, libsodium
, fetchgitPrivate, tryEval, pkgConfig
}:
mkDerivation {
  pname = "hschain-crypto";
  version = "0.1";
  src = tryEval <hschain> (fetchgitPrivate pkgConfig.hschain);
  postUnpack = "sourceRoot+=/hschain-crypto; echo source root reset to $sourceRoot";
  configureFlags = [ "-flibsodium" ];
  libraryHaskellDepends = [
    aeson base base58-bytestring bytestring containers cryptonite
    deepseq entropy Lazy-Pbkdf2 memory primitive serialise SHA text
    vector
  ];
  librarySystemDepends = [ libsodium ];
  doCheck = false;
#  testHaskellDepends = [
#    aeson base base16-bytestring base64-bytestring bytestring serialise
#    tasty tasty-hunit text
#  ];
#  benchmarkHaskellDepends = [
#    base bytestring criterion deepseq random vector
#  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Simple cryptography API for HSChain";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
