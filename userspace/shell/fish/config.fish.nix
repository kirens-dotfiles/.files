{ dotfilesLoc, powerline, bash, ncurses }:
{
  shellInit = ''
    set -g -x DOTFILES "${dotfilesLoc}"
    set PATH (string match -v "/run/current-system/sw/bin" $PATH)
  '';

  # TODO: move to shellAliases and Functions
  interactiveShellInit = import ./aliases.nix.sh {};

  promptInit = import ./prompt.fish.nix {
    powerline = "${powerline}/bin/powerline-go";
    bash = "${bash}/bin/bash";
    tput = "${ncurses}/bin/tput";
  };
}
