{ lib, config, pkgs, ... }: let

  inherit (pkgs) myLib;

  activationPath = /bin/switch-to-configuration;

  /* Wrap nixos activation script in one with a slightly modified interface.
     This is primarilay to create a buffer if anything changes or differs
     slightly in the regular activation script. But it might also prove useful
     if I want to extend funcionality in some way.

     @arg activation path Path to the build activation executable.
     @return derivation A derivation containing `/activate` that is the proxied
                        activation script.
  */
  activationScriptProxy = with myLib.bash; activationBuild: let
    proxy = mkGeneration: message: command: ''
      ${echo} '||' ${lib.escapeShellArg message}
      ${echo} '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'
      ${activationBuild + activationPath} ${command}
      ${lib.optionalString mkGeneration ''
        ${pkgs.nix}/bin/nix-env -p /nix/var/nix/profiles/system \
          --set ${activationBuild}
      ''}
    '';
    start = proxy false;
    store = proxy true;
    activation = pkgs.writeShellScript "activate" (case "$1" [
      "test" (start "Activating build" "test")
      "switch" (store "Switching build" "switch")
      "write" (store "Writing build" "boot")
      "dry-run" (start "Would activate" "dry-activate")
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
in {
  options = {
    entrypoint = lib.mkOption {
      default = activationScriptProxy config.system.build.toplevel;
      description = ''
        The entrypoint into a configuration, generaly an installer or similar.
      '';
      type = lib.types.package;
    };
  };
}
