with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "stripe";
  buildInputs = [
    cabal-install
  ];
}
