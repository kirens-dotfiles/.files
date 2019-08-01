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
      };
    };
  in nixos.system // { inherit (nixos) pkgs config options; };

  activationPath = /bin/switch-to-configuration;

  pkgs = import ./deps/nixpkgs (import ./pkgs);
  inherit (pkgs) stdenv lib myLib;

  /* Wrap nixos activation script in one with a slightly modified interface.
     This is primarilay to create a buffer if anything changes or differs
     slightly in the regular activation script. But it might also prove useful
     if I want to extend funcionality in some way.

     @arg activation path Path to the build activation executable.
     @return derivation A derivation containing `/activate` that is the proxied
                        activation script.
  */
  activationScriptProxy = with myLib.bash; activationBuild: let
    proxy = message: command: ''
      ${echo} '||' ${lib.escapeShellArg message}
      ${echo} '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
      ${activationBuild + activationPath} ${command}
    '';
    activation = pkgs.writeShellScript "activate" (case "$1" [
      "test" (proxy "Activating build" "test")
      "switch" (proxy "Switching build" "switch")
      "write" (proxy "Writing build" "boot")
      "dry-run" (proxy "Would activate" "dry-activate")
      "*" (echoBlock ''
        Usage: ./activate COMMAND

        COMMAND:
          test    Activate build without storeing it on the system.
          write   Store as a generation and replace the boot build.
          switch  Same as `test` followed by `write`.
          dry-run Print what would be activated without doing it.
      '')
    ]);
  in pkgs.runCommand "system-build" { } ''
    mkdir $out
    install ${activation} $out/activate
    ln -s ${activationBuild} $out/build
  ''
  // { build = activationBuild; }
  ;

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
      value = activationScriptProxy (buildConfig name);
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
  ]
  //
  { inherit pkgs; }
