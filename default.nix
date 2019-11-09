# This is the system builder build description
{ }:
let
  buildConfig = name: let
    topLevelPkgs = pkgs;
    nixos = import ./deps/nixpkgs/nixos {
      configuration = { config, lib, ... }: {
        # Use our customized pkgs
        _module.args.pkgs = topLevelPkgs;

        myCfg.dotfilesPath = toString ./.;

        # Imports to add customization
        imports = [
          # Home manager
          ./deps/home-manager/nixos

          # Global modules
          ./modules/env
          ./modules/entrypoint

          # Actual Configuration
          (./configurations + "/${name}")
        ];

        # Link home generations in final symlink tree
        system.extraSystemBuilderCmds = with lib; concatStringsSep "\n" (
          [ "mkdir $out/homes" ]
          ++
          mapAttrsToList
          (name: cfg: "ln -s ${cfg.home.activationPackage} $out/homes/${name}")
          config.home-manager.users
        );

        # Second go overlays, mostly some hidden nixos packages
        nixpkgs.overlays = [ (import ./packages/overlays/second-pass.nix config) ];
      };
    };
  in nixos.config.entrypoint // { inherit (nixos) pkgs config options; };

  pkgs = import ./deps/nixpkgs (import ./packages);
  inherit (pkgs) stdenv lib myLib;

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
