{ pkgs, lib, coreutils }:
let
  removeUnwantedArgs = f: args:
    builtins.intersectAttrs
      (builtins.functionArgs f)
      args;

  callOnlyNessecary = f: args:
    f (removeUnwantedArgs f args);

  callFunction = pkg: {
    text = callOnlyNessecary (import pkg) (pkgs // {
      cut     = "${coreutils}/bin/cut";
      du      = "${coreutils}/bin/du";
      echo    = "${coreutils}/bin/echo";
      mkdir   = "${coreutils}/bin/mkdir";
      mv      = "${coreutils}/bin/mv";
      rm      = "${coreutils}/bin/rm";
      test    = "${coreutils}/bin/test";
      timeout = "${coreutils}/bin/timeout";
    });
  };

  nameToPath = name:
    ".config/fish/functions/" + name + ".fish";

in lib.mapAttrs'
  (n: v: { name = nameToPath n; value = callFunction v; })
  {
    clear-docker = ./clear-docker.nix.fish;
    help-vi = ./help-vi.nix.fish;
    nx-run = ./nx-run.nix.fish;
    prgm = ./prgm.nix.fish;
    trash = ./trash.nix.fish;
    nx-shell = ./nx-shell.nix.fish;
    shell-lvl = ./shell-lvl.nix.fish;
    vol = ./vol.nix.fish;
    lampa = ./lampa.nix.fish;
    pbpaste = ./pbpaste.nix.fish;
    pbcopy = ./pbcopy.nix.fish;
    wifi = ./wifi.nix.fish;
    brightness = ./brightness.nix.fish;
  }

