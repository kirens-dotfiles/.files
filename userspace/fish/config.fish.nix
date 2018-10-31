{ dotfilesLoc }:
{
  shellInit = ''
    set -g -x DOTFILES "${dotfilesLoc}"
  '';

  # TODO: move to shellAliases and Functions
  interactiveShellInit = import ./aliases.sh.nix {};

  promptInit = import ./prompt.fish.nix {};
}
