{ mkDerivation, aeson, base, base58-bytestring, bloodhound
, bytestring, containers, data-default-class, deepseq, directory
, exceptions, filepath, hschain-crypto, hschain-types
, http-client-tls, katip, katip-elasticsearch, lrucache, microlens
, microlens-mtl, microlens-th, mmorph, mtl, network, network-info
, pipes, prometheus-client, random, random-shuffle, retry
, serialise, sqlite-simple, stdenv, stm, text, tls, transformers
, unordered-containers, vector, x509, x509-store, x509-system
, x509-validation
, fetchgitPrivate, tryEval, pkgConfig
}:
mkDerivation {
  pname = "hschain";
  version = "0.1";
  src = tryEval <hschain> (fetchgitPrivate pkgConfig.hschain);
  postUnpack = "sourceRoot+=/hschain; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base base58-bytestring bloodhound bytestring containers
    data-default-class deepseq directory exceptions filepath
    hschain-crypto hschain-types http-client-tls katip
    katip-elasticsearch lrucache microlens microlens-mtl microlens-th
    mmorph mtl network network-info pipes prometheus-client random
    random-shuffle retry serialise sqlite-simple stm text tls
    transformers unordered-containers vector x509 x509-store
    x509-system x509-validation
  ];
  homepage = "https://github.com/hexresearch/thundermint";
  description = "Haskell reimplementation of tendermint protocol";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
