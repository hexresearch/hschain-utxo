{ mkDerivation, base, bytestring, containers, directory, filepath
, process, stdenv, stm, terminfo, transformers, unix
}:
mkDerivation {
  pname = "haskeline";
  version = "0.7.5.0";
  sha256 = "d26508444914efcd6a0369f89c3ade57feae7732c73f5329b610c0c8f1c1dec6";
  revision = "1";
  editedCabalFile = "0i8fyhk7fvz2bxnh5xsmdw5rr7yywzc2wv115034q1g4sb018zrd";
  configureFlags = [ "-fterminfo" ];
  libraryHaskellDepends = [
    base bytestring containers directory filepath process stm terminfo
    transformers unix
  ];
  homepage = "https://github.com/judah/haskeline";
  description = "A command-line interface for user input, written in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
