{ dotfilesLoc }:
builtins.concatStringsSep "\n"
[
''
set -g -x DOTFILES "${dotfilesLoc}"
''

(import ./prompt.fish.nix {})

(import ./aliases.fish.nix {})
]
