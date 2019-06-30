{ stdenv, ghc }:
stdenv.mkDerivation {
  name = "backlight-level-stepper";
  src = ./.;
  buildInputs = [ pkgs.ghc ];
  buildPhase = "ghc main.hs";
  installPhase = "cp main $out";
}
