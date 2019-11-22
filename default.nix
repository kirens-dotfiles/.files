# This is the system builder build description
{ }:
let
  libs = import ./pkgs;
  buildConfig = name: let
    os = libs.nixos {
      extraModules = [ (./configurations + "/${name}") ];
    };
  in os.config.entrypoint // { inherit (os) pkgs config options; };

  pkgs = import ./deps/nixpkgs (import ./packages);
  inherit (libs.pkgs) stdenv lib myLib;

  buildAll = builds: pkgs.runCommand "all-builds" { } (
    myLib.foldSet
      (builder: name: build: builder + "\nln -s ${build} $out/${name}")
      "mkdir $out"
      builds
  );

  # Combine builder, activation and runner.
  availableBuilds = names: let
    builds = lib.listToAttrs (lib.flip map names (name: {
      inherit name;
      value = buildConfig name;
    }));
  in
    builds
    # Make all builds in one go and link them by their respective names in root
    // { all = buildAll builds; }
    # Merge configurations with a dummy package that casues an evaluation
    # error. This will give help a user trying to run `nix build` without any
    # aditional arguments.
    // myLib.derivationError ''

      ' Specify which derivation you want to build!
      ' Use `nix-build -A UNIT`, where UNIT is one of:
      '  "${lib.concatStringsSep "\"\n'  \"" names}"
    '';
in
  availableBuilds [
    "laptop"
    "installer"
    "cerberus"
  ]
  //
  { inherit pkgs; }
