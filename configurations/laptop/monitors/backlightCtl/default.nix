{ stdenv, ghc }:
stdenv.mkDerivation {
  name = "backlight-level-stepper";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = "ghc main.hs";
  installPhase = "cp main $out";
}
