# This is the system builder build description
{ }:
let
  buildConfig = name: let
    nixos = import ./deps/nixpkgs/nixos {
      configuration = ./configurations + "/${name}";
    };
  in nixos.system;

  activationPath = /bin/switch-to-configuration;

  pkgs = import ./deps/nixpkgs { };
  inherit (pkgs) stdenv lib;

  # Some helpers
  mapLines = mapper: text:
    lib.concatMapStringsSep "\n" mapper (lib.splitString "\n" text);

  # Some bash shorthands
  bash = with lib; rec {
    echo = "${pkgs.coreutils}/bin/echo";

    echoBlock = mapLines (line: "${echo} ${escapeShellArg line}");

    case = let
      mapCaseList' = mapper: foldl ({ result, prop }: val:
        if isNull prop
        then { inherit result; prop = val; }
        else { result = result ++ [(mapper prop val)]; prop = null; }
      ) { result = [ ]; prop = null; };
      mapCaseList = mapper: list: (mapCaseList' mapper list).result;
      caseToString = case: body: ''
        ${case})
          ${body}
          ;;
      '';
    in on: cases: ''
      case ${on} in
          ${concatStringsSep "\n" (mapCaseList caseToString cases)}
      esac
    '';
  };

  /* Wrap nixos activation script in one with a slightly modified interface.
     This is primarilay to create a buffer if anything changes or differs
     slightly in the regular activation script. But it might also prove useful
     if I want to extend funcionality in some way.

     @arg activation path Path to the build activation executable.
     @return derivation A derivation containing `/activate` that is the proxied
                        activation script.
  */
  activationScriptProxy = activation: let
    proxy = message: command: ''
      ${bash.echo} '||' ${lib.escapeShellArg message}
      ${bash.echo} '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
      ${activation} ${command}
    '';
  in pkgs.writeShellScript "activate" (bash.case "$1" [
    "test" (proxy "Activating build" "test")
    "switch" (proxy "Switching build" "switch")
    "write" (proxy "Writing build" "boot")
    "dry-run" (proxy "Would activate" "dry-activate")
    "*" (bash.echoBlock ''
      Usage: ./activate COMMAND

      COMMAND:
        test    Activate build without storeing it on the system.
        write   Store as a generation and replace the boot build.
        switch  Same as `test` followed by `write`.
        dry-run Print what would be activated without doing it.
    '')
  ]);

  derivationError = msg: stdenv.mkDerivation { name = abort msg; };

  # Combine builder, activation and runner.
  availableBuilds = names:
    lib.listToAttrs (lib.flip map names (name: {
      inherit name;
      value = activationScriptProxy (buildConfig name + activationPath);
    }))
    # Merge configurations with a dummy package that casues an evaluation
    # error. This will give help a user trying to run `nix build` without any
    # aditional arguments.
    // derivationError ''

      ' Specify which derivation you want to build!
      ' Use `nix-build -A UNIT`, where UNIT is one of:
      '  "${lib.concatStringsSep "\"\n'  \"" names}"
    '';
in
  availableBuilds [
    "laptop"
  ]
