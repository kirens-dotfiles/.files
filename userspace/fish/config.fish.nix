{ dotfilesLoc }:
{
  interactiveShellInit =
    builtins.concatStringsSep "\n"
    [
      ''
      set -g -x DOTFILES "${dotfilesLoc}"
      ''

      (import ./prompt.fish.nix {})

      (import ./aliases.sh.nix {})
    ];
}
