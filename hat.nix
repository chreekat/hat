{ mkDerivation, aeson, array, async, base, bytestring, cborg
, containers, directory, filepath, hspec, lib, libghostty-vt
, megaparsec, network, process, QuickCheck, serialise
, sqlite-simple, stm, text, time, unix, vector
}:
mkDerivation {
  pname = "hat";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base bytestring cborg containers directory
    filepath megaparsec network process serialise sqlite-simple stm
    text time unix vector
  ];
  libraryPkgconfigDepends = [ libghostty-vt ];
  executableHaskellDepends = [
    base bytestring directory network process text unix vector
  ];
  testHaskellDepends = [
    async base bytestring cborg containers directory filepath hspec
    network process QuickCheck serialise sqlite-simple stm text time
    unix vector
  ];
  description = "A terminal multiplexer";
  license = lib.meta.getLicenseFromSpdxId "AGPL-3.0-or-later";
}
