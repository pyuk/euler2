{ mkDerivation, base, primes, stdenv }:
mkDerivation {
  pname = "euler2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base primes ];
  license = stdenv.lib.licenses.bsd3;
}
