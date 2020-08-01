{ mkDerivation, aeson, array, async, base, bytestring, cmdargs
, containers, hpack, HTTP, lens, megaparsec, optparse-applicative
, process, random, rio, stdenv, tagsoup, text, time, transformers
, wreq, yaml
}:
mkDerivation {
  pname = "chefkoch";
  version = "2.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base bytestring cmdargs containers HTTP lens
    megaparsec process random rio tagsoup text time transformers wreq
    yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cmdargs megaparsec optparse-applicative tagsoup
    text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/TobiasPleyer/chefkoch#readme";
  license = stdenv.lib.licenses.bsd3;
}
