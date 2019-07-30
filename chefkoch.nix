{ mkDerivation, aeson, array, async, base, bytestring, cmdargs
, containers, hpack, HTTP, lens, megaparsec, optparse-applicative
, process, random, rio, stdenv, tagsoup, text, time, wreq, yaml
}:
mkDerivation {
  pname = "chefkoch";
  version = "1.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base bytestring cmdargs containers HTTP lens
    megaparsec process random rio tagsoup text time wreq yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cmdargs megaparsec optparse-applicative tagsoup
    text
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/TobiasPleyer/chefkoch#readme";
  license = stdenv.lib.licenses.bsd3;
}
