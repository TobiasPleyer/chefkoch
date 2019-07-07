{ mkDerivation, aeson, array, async, base, bytestring, hpack, HTTP
, lens, optparse-applicative, process, random, stdenv, tagsoup
, text, time, wreq, yaml
}:
mkDerivation {
  pname = "chefkoch";
  version = "1.1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base bytestring HTTP lens process random tagsoup
    text time wreq yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring optparse-applicative text
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/TobiasPleyer/chefkoch#readme";
  license = stdenv.lib.licenses.bsd3;
}
