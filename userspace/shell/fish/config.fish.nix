{ dotfilesLoc, powerline, bash, ncurses, coreutils }:
{
  shellAbbrs= {
    # Prevent unintentional overwrites
    mv = "mv -i";
    cp = "cp -i";
    cpr = "cp -riv";

    # Git shorthands
    gb = "git branch";
    gbd = "git branch -d";
    gs = "git status";
    gsu = "gs -uno";
    ga = "git add";
    gai = "git add -i";
    gch = "git checkout";
    gchb = "git checkout -b";
    gc = "git commit";
    gcm = "git commit -m";
    gcam = "git commit -am";
    gf = "git fetch";
    gm = "git merge";
    gpl = "git pull";
    gplu = "git pull upstream (git rev-parse --abbrev-ref HEAD)";
    gp = "git push";
    gpu = "git-pushu";
    gpuo = "git-pushu origin";
    gd = "git diff";
    gl = "git log";
    gln = "git log -n";
    gl1 = "git log -n 1";
    gprune = "git fetch --prune";

    # View files
    r = "vi -R";

    # Move about and looking around
    l = "ls -lAh";
    la = "ls -A";
    ld = "ls -ld";

    # Copy paste pipe
    cpy = "pbcopy";
    pst = "pbpaste";

    # Nixos shorthands
    nix-system = "sudo nix-env -p /nix/var/nix/profiles/system";
    nix-bootentry = "sudo nixos-rebuild switch -p";
  };

  shellInit = ''
    set -g -x DOTFILES "${dotfilesLoc}"
    for path in \
        /run/current-system/sw/bin \
        /home/kiren/.nix-profile/bin \
        /run/wrappers/bin
      if not contains $path $PATH
        set -p PATH "$path"
      end
    end
  '';

  interactiveShellInit =
    import ./editCmd.nix.fish {
      inherit coreutils;
      vim = "vi"; # TODO: should be an absolute reference
    };

  promptInit = import ./prompt.fish.nix {
    powerline = "${powerline}/bin/powerline-go";
    bash = "${bash}/bin/bash";
    tput = "${ncurses}/bin/tput";
  };
}
