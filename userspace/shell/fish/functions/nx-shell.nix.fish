{ nix, echo, fish }:
''
function nx-shell
  ${nix}/bin/nix-shell \
    -I nixpkgs=$DOTFILES/nix/src/nixpkgs/ \
    --command "${fish}/bin/fish" $argv

  ${echo} Current shell depth is (shell-lvl)
end
''
