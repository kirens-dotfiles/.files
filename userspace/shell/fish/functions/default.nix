{ pkgs, lib, coreutils }:
let
  removeUnwantedArgs = f: args:
    builtins.intersectAttrs
      (builtins.functionArgs f)
      args;

  callOnlyNessecary = f: args:
    f (removeUnwantedArgs f args);

  callFunction = pkg: callOnlyNessecary (import pkg) (pkgs // {
    cut     = "${coreutils}/bin/cut";
    du      = "${coreutils}/bin/du";
    echo    = "${coreutils}/bin/echo";
    mkdir   = "${coreutils}/bin/mkdir";
    mktemp  = "${coreutils}/bin/mktemp";
    mv      = "${coreutils}/bin/mv";
    rm      = "${coreutils}/bin/rm";
    tail    = "${coreutils}/bin/tail";
    test    = "${coreutils}/bin/test";
    timeout = "${coreutils}/bin/timeout";
  });

in lib.mapAttrsToList
  (name: source: pkgs.writeTextFile {
    name = "${name}.fish";
    destination = "/share/fish/functions/${name}.fish";
    text = callFunction source;
  })
  {
    clear-docker = ./clear-docker.nix.fish;
    git-pushu = ./git-pushu.nix.fish;
    git-tmpclone = ./git-tmpclone.nix.fish;
    help-vi = ./help-vi.nix.fish;
    prgm = ./prgm.nix.fish;
    trash = ./trash.nix.fish;
    nx-shell = ./nx-shell.nix.fish;
    tmpdir = ./tmpdir.nix.fish;
    try = ./try.nix.fish;
    tryRun = ./tryRun.nix.fish;
    shell-lvl = ./shell-lvl.nix.fish;
    vol = ./vol.nix.fish;
    lampa = ./lampa.nix.fish;
    pbpaste = ./pbpaste.nix.fish;
    pbcopy = ./pbcopy.nix.fish;
    wifi = ./wifi.nix.fish;
    brightness = ./brightness.nix.fish;
  }
